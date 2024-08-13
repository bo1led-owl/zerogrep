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

    const read_buffer = try gpa.alloc(u8, 1 * KiB);
    defer gpa.free(read_buffer);

    if (!args.from_stdin) {
        const cwd = std.fs.cwd();

        const multiple_files = args.filenames.len > 0;
        for (0.., args.filenames) |i, filename| {
            const file = cwd.openFile(filename, .{}) catch |e| {
                try stderr.print("Error opening `{s}`: {s}\n", .{ filename, @errorName(e) });
                continue;
            };
            defer file.close();

            var buffered_reader = std.io.bufferedReader(file.reader());
            var reader = buffered_reader.reader();

            const last_file = i == args.filenames.len - 1;
            const flags = HandleFileFlags{
                .data_only = args.data_only,
                .multiple_files = multiple_files,
                .last_file = last_file,
            };
            try handleFile(filename, &reader, read_buffer, stdout, nfa, &nfa_stack, flags);
        }
    } else {
        const stdin_file = std.io.getStdIn();

        var buffered_reader = std.io.bufferedReader(stdin_file.reader());
        var reader = buffered_reader.reader();

        const flags = HandleFileFlags{
            .data_only = args.data_only,
            .multiple_files = false,
            .last_file = true,
        };
        try handleFile("stdin", &reader, read_buffer, stdout, nfa, &nfa_stack, flags);
    }
    try bw_stdout.flush();
    return ExitCode.Success;
}

const HandleFileFlags = struct {
    data_only: bool,
    multiple_files: bool,
    last_file: bool,
};

fn handleFile(filename: []const u8, reader: anytype, buffer: []u8, stdout: anytype, nfa: NFA, nfa_stack: *NFA.Stack, flags: HandleFileFlags) !void {
    var line_number: u32 = 1;
    var printed_filename = false;
    var printed = false;

    while (try reader.readUntilDelimiterOrEof(buffer, '\n')) |line| : (line_number += 1) {
        if (!try nfa.match(nfa_stack, line)) {
            continue;
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

    if (printed and !flags.data_only and flags.multiple_files and !flags.last_file) {
        try stdout.writeByte('\n');
    }
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
