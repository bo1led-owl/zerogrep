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
    const stdout_file = std.io.getStdOut();
    var bw_stdout = std.io.bufferedWriter(stdout_file.writer());
    const stdout = bw_stdout.writer();

    var args: cli.Args = undefined;
    defer args.deinit(gpa);

    var from_stdin = false;

    {
        const stdin_is_tty = std.io.getStdIn().isTty();
        const stdout_is_tty = stdout_file.isTty();
        var cli_result = try cli.Args.parse(gpa, arena, stdin_is_tty, stdout_is_tty);
        defer cli_result.errors.deinit();

        args = cli_result.args;

        if (args.print_help) {
            try cli.printHelp(stdout);
            try bw_stdout.flush();
            return ExitCode.Success;
        }

        if (args.paths.len == 0) {
            if (!stdin_is_tty) {
                from_stdin = true;
            } else {
                try cli_result.errors.addError("No input provided", .{}, {});
            }
        }

        if (cli_result.errors.count() != 0) {
            for (cli_result.errors.items()) |err| {
                try stderr.print("Error: {s}\n", .{err.message});
            }
            return ExitCode.IncorrectUsage;
        }
    }

    // var timer = try std.time.Timer.start();

    var strategy: SearchStrategy = undefined;
    defer strategy.deinit(gpa);
    {
        var regex = Regex.init(args.pattern);
        const string_literal = try regex.toStringLiteral(gpa);
        if (string_literal) |l| {
            strategy = SearchStrategy.initStringLiteral(l);
        } else {
            var nfa_build_result = try regex.buildNFA(gpa, arena);
            defer nfa_build_result.errors.deinit();

            // const nfa_build_time: f64 = @floatFromInt(timer.read());
            // std.debug.print("NFA is built in {d:.3}us\n", .{
            //     nfa_build_time / std.time.ns_per_us,
            // });

            strategy = SearchStrategy.initRegex(gpa, nfa_build_result.automata);
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
    }

    // nfa.debugPrint();

    // var read_buffer = try std.ArrayList(u8).initCapacity(gpa, 96 * KiB);
    // defer read_buffer.deinit();

    const read_buffer = try gpa.alloc(u8, 96 * KiB);
    defer gpa.free(read_buffer);

    var code = ExitCode.Success;
    if (!from_stdin) {
        const cwd = std.fs.cwd();

        if (args.recursive) {
            try checkRecursively(gpa, args, cwd, stdout, stderr, read_buffer, &strategy);
        } else {
            const multiple_files = args.paths.len > 1;
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
                if (try handleFile(file, path, read_buffer, stdout, &strategy, flags)) {
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
        _ = try handleFile(stdin_file, "stdin", read_buffer, stdout, &strategy, flags);
    }
    try bw_stdout.flush();
    return code;
}

const SearchStrategy = union(enum) {
    StringLiteral: []const u8,
    Regex: struct {
        nfa: NFA,
        nfa_stack: NFA.Stack,
    },

    pub fn initStringLiteral(lit: []const u8) SearchStrategy {
        return SearchStrategy{
            .StringLiteral = lit,
        };
    }

    pub fn initRegex(gpa: std.mem.Allocator, nfa: NFA) SearchStrategy {
        return SearchStrategy{
            .Regex = .{
                .nfa = nfa,
                .nfa_stack = NFA.Stack.init(gpa),
            },
        };
    }

    pub fn deinit(self: *SearchStrategy, gpa: std.mem.Allocator) void {
        switch (self.*) {
            .StringLiteral => |l| {
                gpa.free(l);
            },
            .Regex => |*r| {
                r.*.nfa.deinit(gpa);
                r.*.nfa_stack.deinit();
            },
        }
    }

    pub fn match(self: *SearchStrategy, line: []const u8) !bool {
        switch (self.*) {
            .StringLiteral => |l| {
                return std.mem.indexOf(u8, line, l) != null;
            },
            .Regex => |*r| {
                return r.*.nfa.match(&r.*.nfa_stack, line);
            },
        }
    }
};

pub fn checkRecursively(gpa: std.mem.Allocator, args: cli.Args, cwd: std.fs.Dir, stdout: anytype, stderr: anytype, read_buffer: []u8, strategy: *SearchStrategy) !void {
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
                if (try handleFile(file, path, read_buffer, stdout, strategy, flags)) {
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

                            if (try handleFile(file, entry.path, read_buffer, stdout, strategy, flags)) {
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

fn LineReader(comptime ReaderType: type) type {
    return struct {
        const Self = @This();

        unbuffered_reader: ReaderType,
        buffer: []u8,
        start: usize,
        end: usize,

        pub fn preread(self: *Self) !void {
            std.debug.assert(self.start == 0);
            std.debug.assert(self.end == 0);
            self.end = try self.unbuffered_reader.readAll(self.buffer);
        }

        pub fn cur(self: Self) []const u8 {
            return self.buffer[self.start..self.end];
        }

        pub fn next(self: *Self) !?[]const u8 {
            var newline_index = std.mem.indexOfScalarPos(u8, self.buffer[0..self.end], self.start, '\n');
            if (newline_index == null) {
                const tail_len = self.end - self.start;
                @memcpy(self.buffer[0..tail_len], self.buffer[self.start..self.end]);
                self.end = try self.unbuffered_reader.readAll(self.buffer);
                self.start = 0;
                newline_index = std.mem.indexOfScalar(u8, self.buffer[0..self.end], '\n');
            }

            if (self.start == self.end and newline_index == null) {
                return null;
            }

            defer self.start = newline_index.? + 1;
            return self.buffer[self.start..newline_index.?];
        }
    };
}

fn lineReader(buffer: []u8, reader: anytype) LineReader(@TypeOf(reader)) {
    return LineReader(@TypeOf(reader)){
        .unbuffered_reader = reader,
        .buffer = buffer,
        .start = 0,
        .end = 0,
    };
}

fn handleFile(file: std.fs.File, filename: []const u8, buffer: []u8, stdout: anytype, strategy: *SearchStrategy, flags: HandleFileFlags) !bool {
    var line_reader = lineReader(buffer, file.reader());
    try line_reader.preread();

    // std.debug.print("{s}\n", .{filename});
    if (try isFileBinary(line_reader.cur())) {
        return false;
    }

    var line_number: u32 = 1;
    var printed = false;

    while (try line_reader.next()) |line| : (line_number += 1) {
        if (try handleLine(stdout, filename, line_number, strategy, line, printed, flags)) {
            printed = true;
        }
    }

    return printed;
}

fn handleLine(stdout: anytype, filename: []const u8, line_number: u32, strategy: *SearchStrategy, line: []const u8, printed: bool, flags: HandleFileFlags) !bool {
    if (!try strategy.match(line)) {
        return false;
    }

    if (!printed and !flags.data_only and flags.multiple_files and !flags.first_file) {
        try stdout.writeByte('\n');
    }

    if (!flags.data_only) {
        const should_print_filename = !flags.data_only and flags.multiple_files and !printed;
        const should_print_line_number = !flags.data_only;

        if (should_print_filename) {
            try stdout.print("{s}{s}{s}\n", .{ cli.ANSI.Fg.Magenta, filename, cli.ANSI.Reset });
        }

        if (should_print_line_number) {
            try stdout.print("{s}{d}:{s} ", .{ cli.ANSI.Fg.Green, line_number, cli.ANSI.Reset });
        }
    }

    try stdout.print("{s}\n", .{line});
    return true;
}

fn isFileBinary(buf: []const u8) !bool {
    var printable: usize = 0;
    var zeros: usize = 0;

    const control_chunk = buf[0..@min(buf.len, 1 * KiB)];
    for (control_chunk) |c| {
        printable += @as(usize, @intFromBool(std.ascii.isPrint(c)));
        zeros += @as(usize, @intFromBool(c == 0));
    }

    const printable_percentage = @as(f64, @floatFromInt(printable)) / @as(f64, @floatFromInt(control_chunk.len)) * 100.0;
    const zeros_percentage = @as(f64, @floatFromInt(zeros)) / @as(f64, @floatFromInt(control_chunk.len)) * 100.0;

    // std.debug.print("{d} {d}\n", .{printable_percentage, zeros_percentage});
    if (printable_percentage < 20.0 or zeros_percentage > 2.0) {
        return true;
    }

    return false;
}

test {
    std.testing.refAllDeclsRecursive(@This());
}
