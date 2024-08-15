const std = @import("std");
const cli = @import("cli.zig");

const Regex = @import("Regex.zig");
const NFA = @import("NFA.zig");
const Errors = @import("errors.zig").Errors;

const KiB = 1024;
const MiB = 1024 * KiB;
const GiB = 1024 * MiB;

const ExitCode = enum(u8) {
    Success = 0,
    GenericError = 1,
    IncorrectUsage = 2,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        std.debug.assert(status != .leak);
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stderr_file = std.io.getStdErr().writer();
    var bw_stderr = std.io.bufferedWriter(stderr_file);
    const stderr = bw_stderr.writer();

    const code = run(allocator, arena.allocator(), stderr) catch |e| blk: {
        try stderr.print("Error: {s}\n", .{@errorName(e)});
        break :blk ExitCode.GenericError;
    };
    try bw_stderr.flush();

    std.process.exit(@intFromEnum(code));
}

fn run(gpa: std.mem.Allocator, arena: std.mem.Allocator, stderr: anytype) !ExitCode {
    const stdout_file = std.io.getStdOut().writer();
    var bw_stdout = std.io.bufferedWriter(stdout_file);
    const stdout = bw_stdout.writer();

    var args: cli.Args = undefined;
    defer args.deinit(gpa);

    {
        var cli_result = try cli.Args.parse(gpa, arena);
        defer cli_result.errors.deinit();

        args = cli_result.args;

        if (args.print_help) {
            try cli.printHelp(stdout);
            try bw_stdout.flush();
            return ExitCode.Success;
        }

        if (cli_result.errors.count() != 0) {
            for (cli_result.errors.items()) |err| {
                try stderr.print("Error: {s}\n", .{err.message});
            }
            return ExitCode.IncorrectUsage;
        }
    }

    // var timer = try std.time.Timer.start();

    var nfa: NFA = undefined;
    defer nfa.deinit();

    {
        var regex = Regex.init(args.pattern);
        var nfa_build_result = try regex.buildNFA(gpa, arena);
        defer nfa_build_result.errors.deinit();

        // const nfa_build_time: f64 = @floatFromInt(timer.read());
        // std.debug.print("NFA is built in {d:.3}us\n", .{
        //     nfa_build_time / std.time.ns_per_us,
        // });

        nfa = nfa_build_result.automata;
        if (nfa_build_result.errors.count() != 0) {
            for (nfa_build_result.errors.items()) |err| {
                try stderr.print("{s}\n", .{args.pattern});

                const span = err.payload;

                try stderr.writeByteNTimes(' ', span.start);
                try stderr.writeAll(cli.ANSI.Fg.Red ++ cli.ANSI.Bold);
                try stderr.writeByteNTimes('^', span.end - span.start + 1);
                try stderr.print(" {s}{s}\n", .{ err.message, cli.ANSI.Reset });
            }
            return ExitCode.IncorrectUsage;
        }
    }

    // nfa.debugPrint();

    var nfa_stack = NFA.Stack.init(gpa);
    defer nfa_stack.deinit();

    var read_buffer = try std.ArrayList(u8).initCapacity(gpa, 4 * KiB);
    defer read_buffer.deinit();

    var code = ExitCode.Success;
    if (!args.from_stdin) {
        const cwd = std.fs.cwd();

        if (args.recursive) {
            try checkRecursively(gpa, args, cwd, stdout, stderr, &read_buffer, nfa, &nfa_stack);
        } else {
            const multiple_files = args.paths.len > 0;
            var first_file = true;
            for (args.paths) |path| {
                const file = cwd.openFile(path, .{}) catch |e| {
                    try stderr.print("Error opening `{s}`: {s}\n", .{ path, @errorName(e) });
                    code = ExitCode.GenericError;
                    continue;
                };
                defer file.close();

                const flags = HandleFileFlags{
                    .data_only = args.data_only,
                    .multiple_files = multiple_files,
                    .first_file = first_file,
                };
                if (try handleFile(file, path, &read_buffer, stdout, nfa, &nfa_stack, flags)) {
                    first_file = true;
                }
            }
        }
    } else {
        const stdin_file = std.io.getStdIn();

        const flags = HandleFileFlags{
            .data_only = args.data_only,
            .multiple_files = false,
            .first_file = true,
        };
        _ = try handleFile(stdin_file, "stdin", &read_buffer, stdout, nfa, &nfa_stack, flags);
    }
    try bw_stdout.flush();
    return code;
}

pub fn checkRecursively(gpa: std.mem.Allocator, args: cli.Args, cwd: std.fs.Dir, stdout: anytype, stderr: anytype, read_buffer: *std.ArrayList(u8), nfa: NFA, nfa_stack: *NFA.Stack) !void {
    var first_file = true;
    for (args.paths) |path| {
        const filestat = cwd.statFile(path) catch |e| {
            try stderr.print("Error trying to stat `{s}`: {s}\n", .{ path, @errorName(e) });
            continue;
        };

        switch (filestat.kind) {
            .file => {
                const file = cwd.openFile(path, .{}) catch |e| {
                    try stderr.print("Error opening `{s}`: {s}\n", .{ path, @errorName(e) });
                    continue;
                };
                defer file.close();

                const flags = HandleFileFlags{
                    .data_only = args.data_only,
                    .multiple_files = true,
                    .first_file = first_file,
                };
                if (try handleFile(file, path, read_buffer, stdout, nfa, nfa_stack, flags)) {
                    first_file = false;
                }
            },
            .directory => {
                var dir = cwd.openDir(path, .{
                    .iterate = true,
                }) catch |e| {
                    try stderr.print("Error opening `{s}`: {s}\n", .{ path, @errorName(e) });
                    continue;
                };
                defer if (cwd.fd != dir.fd) {
                    dir.close();
                };

                var iter = try dir.walk(gpa);
                defer iter.deinit();
                while (try iter.next()) |entry| {
                    switch (entry.kind) {
                        .file => {
                            const file = entry.dir.openFile(entry.basename, .{}) catch |e| {
                                try stderr.print("Error opening `{s}`: {s}\n", .{ entry.path, @errorName(e) });
                                continue;
                            };
                            defer file.close();

                            const flags = HandleFileFlags{
                                .data_only = args.data_only,
                                .multiple_files = true,
                                .first_file = first_file,
                            };

                            if (try handleFile(file, entry.path, read_buffer, stdout, nfa, nfa_stack, flags)) {
                                first_file = false;
                            }
                        },
                        else => {},
                    }
                }
            },
            else => {},
        }
    }
}

const HandleFileFlags = struct {
    data_only: bool,
    multiple_files: bool,
    first_file: bool,
};

fn handleFile(file: std.fs.File, filename: []const u8, buffer: *std.ArrayList(u8), stdout: anytype, nfa: NFA, nfa_stack: *NFA.Stack, flags: HandleFileFlags) !bool {
    if (try isFileBinary(file)) {
        return false;
    }

    var buffered_reader = std.io.bufferedReader(file.reader());
    var reader = buffered_reader.reader();

    const ReaderT = @TypeOf(reader);

    var line_number: u32 = 1;
    var printed_filename = false;
    var printed = false;
    var running = true;

    while (running) : (line_number += 1) {
        reader.streamUntilDelimiter(buffer.writer(), '\n', 128 * MiB) catch |e| switch (e) {
            ReaderT.NoEofError.EndOfStream => running = false,
            else => return e,
        };
        defer buffer.clearRetainingCapacity();

        const line = buffer.items;
        if (!try nfa.match(nfa_stack, line)) {
            continue;
        }

        if (!printed_filename and !flags.data_only and flags.multiple_files and !flags.first_file) {
            try stdout.writeByte('\n');
        }

        if (!flags.data_only) {
            const should_print_filename = flags.multiple_files and !printed_filename;
            const should_print_line_number = flags.multiple_files;

            if (should_print_filename) {
                try stdout.print("{s}{s}{s}\n", .{ cli.ANSI.Fg.Magenta, filename, cli.ANSI.Reset });
                printed_filename = true;
            }

            if (should_print_line_number) {
                try stdout.print("{s}{d}:{s} ", .{ cli.ANSI.Fg.Green, line_number, cli.ANSI.Reset });
            }
        }

        try stdout.print("{s}\n", .{line});
        printed = true;
    }

    return printed;
}

fn isFileBinary(file: std.fs.File) !bool {
    const STARTING_CHUNK_SIZE = 1 * KiB;
    var starting_chunk: [STARTING_CHUNK_SIZE]u8 = undefined;
    const bytes_read = try file.readAll(&starting_chunk);

    var printable: usize = 0;
    var zeros: usize = 0;
    for (starting_chunk[0..bytes_read]) |c| {
        printable += @as(usize, @intFromBool(std.ascii.isPrint(c)));
        zeros += @as(usize, @intFromBool(c == 0));
    }

    const printable_percentage = @as(f32, @floatFromInt(printable)) / @as(f32, @floatFromInt(STARTING_CHUNK_SIZE)) * 100.0;
    const zeros_percentage = @as(f32, @floatFromInt(zeros)) / @as(f32, @floatFromInt(STARTING_CHUNK_SIZE)) * 100.0;
    if (printable_percentage < 70.0 or zeros_percentage > 5.0) {
        return true;
    }

    return false;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
