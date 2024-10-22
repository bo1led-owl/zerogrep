const std = @import("std");
const cli = @import("cli.zig");

const Regex = @import("Regex.zig");
const NFA = @import("NFA.zig");
const DFA = @import("DFA.zig");
const DfaBuilder = @import("DfaBuilder.zig");
const Errors = @import("errors.zig").Errors;

const buildDfaFromNfa = @import("determinization.zig").buildDfaFromNfa;
const minimizeDFA = @import("DFA_minimization.zig").minimizeDFA;

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

    var error_msg_arena = std.heap.ArenaAllocator.init(allocator);
    defer error_msg_arena.deinit();

    const stderr_file = std.io.getStdErr().writer();
    var bw_stderr = std.io.bufferedWriter(stderr_file);
    const stderr = bw_stderr.writer();

    const code = run(allocator, error_msg_arena.allocator(), stderr) catch |e| blk: {
        try stderr.print("Error: {s}\n", .{@errorName(e)});
        break :blk ExitCode.GenericError;
    };
    try bw_stderr.flush();

    std.process.exit(@intFromEnum(code));
}

fn run(gpa: std.mem.Allocator, error_msg_arena: std.mem.Allocator, stderr: anytype) !ExitCode {
    const stdout_file = std.io.getStdOut();
    var bw_stdout = std.io.bufferedWriter(stdout_file.writer());
    const stdout = bw_stdout.writer();

    var from_stdin = false;

    var args = args_blk: {
        const stdin_is_tty = std.io.getStdIn().isTty();
        const stdout_is_tty = stdout_file.isTty();
        var cli_result = try cli.Args.parse(gpa, error_msg_arena, stdin_is_tty, stdout_is_tty);
        defer cli_result.errors.deinit();

        if (cli_result.args.print_help) {
            try cli.printHelp(stdout);
            try bw_stdout.flush();
            cli_result.args.deinit(gpa);
            return ExitCode.Success;
        }

        if (cli_result.errors.count() != 0) {
            for (cli_result.errors.items()) |err| {
                try stderr.print("Error: {s}\n", .{err.message});
            }
            cli_result.args.deinit(gpa);
            return ExitCode.IncorrectUsage;
        }

        if (cli_result.args.paths.len == 0 and !stdin_is_tty) {
            from_stdin = true;
        }

        break :args_blk cli_result.args;
    };
    defer args.deinit(gpa);

    var strategy: SearchStrategy = strategy_blk: {
        var regex = Regex.init(args.pattern);
        const string_literal = try regex.toStringLiteral(gpa);
        if (string_literal) |l| {
            break :strategy_blk SearchStrategy.initStringLiteral(l);
        }

        var nfa_build_result = try regex.buildNFA(gpa, error_msg_arena);
        defer nfa_build_result.errors.deinit();

        if (nfa_build_result.errors.count() != 0) {
            for (nfa_build_result.errors.items()) |err| {
                try reportRegexError(stderr, args.pattern, err.payload, err.message);
            }
            nfa_build_result.automaton.deinit(gpa);
            return ExitCode.IncorrectUsage;
        }

        // TODO: limit DFA size, roll back to NFA in critical situations
        // break :strategy_blk SearchStrategy.initNFA(gpa, nfa_build_result.automaton);

        var dfa = try buildDfaFromNfa(gpa, nfa_build_result.automaton);
        defer dfa.deinit(gpa);

        var minimization_arena = std.heap.ArenaAllocator.init(gpa);
        defer minimization_arena.deinit();

        const min_dfa = try minimizeDFA(gpa, minimization_arena.allocator(), dfa);
        break :strategy_blk SearchStrategy.initDFA(min_dfa);
    };
    defer strategy.deinit(gpa);

    const read_buffer = try gpa.alloc(u8, 96 * KiB);
    defer gpa.free(read_buffer);

    var exit_code = ExitCode.Success;
    if (!from_stdin) {
        const cwd = std.fs.cwd();
        exit_code = try checkRecursively(gpa, args, cwd, stdout, stderr, read_buffer, &strategy);
    } else {
        const stdin_file = std.io.getStdIn();

        const flags = HandleFileFlags{
            .multiple_files = false,
            .first_file = true,
            .line_numbers = args.line_numbers,
            .filenames = false,
            .color = args.color,
        };
        _ = try handleFile(flags, stdin_file, "stdin", &strategy, stdout, read_buffer);
    }

    try bw_stdout.flush();
    return exit_code;
}

fn reportRegexError(stderr: anytype, pattern: []const u8, span: Regex.SourceSpan, message: []const u8) !void {
    try stderr.print("{s}\n", .{pattern});

    try stderr.writeByteNTimes(' ', span.start);
    try stderr.writeAll(cli.ANSI.Fg.Red ++ cli.ANSI.Bold);
    try stderr.writeByteNTimes('^', span.end - span.start + 1);
    try stderr.print(" {s}{s}\n", .{ message, cli.ANSI.Reset });
}

const SearchStrategy = union(enum) {
    StringLiteral: []const u8,
    NFA: struct {
        nfa: NFA,
        nfa_stack: NFA.Stack,
    },
    DFA: struct {
        dfa: DFA,
    },

    pub fn initStringLiteral(lit: []const u8) SearchStrategy {
        return SearchStrategy{
            .StringLiteral = lit,
        };
    }

    pub fn initNFA(gpa: std.mem.Allocator, nfa: NFA) SearchStrategy {
        return SearchStrategy{
            .NFA = .{
                .nfa = nfa,
                .nfa_stack = NFA.Stack.init(gpa),
            },
        };
    }

    pub fn initDFA(dfa: DFA) SearchStrategy {
        return SearchStrategy{
            .DFA = .{
                .dfa = dfa,
            },
        };
    }

    pub fn deinit(self: *SearchStrategy, gpa: std.mem.Allocator) void {
        switch (self.*) {
            .StringLiteral => |l| {
                gpa.free(l);
            },
            .NFA => |*n| {
                n.*.nfa.deinit(gpa);
                n.*.nfa_stack.deinit();
            },
            .DFA => |*d| {
                d.*.dfa.deinit(gpa);
            },
        }
    }

    pub fn match(self: *SearchStrategy, lazy: bool, line: []const u8) !?Regex.MatchResult {
        switch (self.*) {
            .StringLiteral => |l| {
                if (std.mem.indexOf(u8, line, l)) |index| {
                    return .{ .start = @intCast(index), .end = @intCast(index + l.len) };
                } else return null;
            },
            .NFA => |*n| {
                return try n.*.nfa.match(&n.*.nfa_stack, line);
            },
            .DFA => |d| {
                return d.dfa.match(lazy, line);
            },
        }
    }
};

pub fn checkRecursively(gpa: std.mem.Allocator, args: cli.Args, cwd: std.fs.Dir, stdout: anytype, stderr: anytype, read_buffer: []u8, strategy: *SearchStrategy) !ExitCode {
    var flags = HandleFileFlags{
        .multiple_files = true,
        .first_file = true,
        .line_numbers = args.line_numbers,
        .filenames = args.filenames,
        .color = args.color,
    };

    for (args.paths) |path| {
        const exit_code = try handlePath(gpa, path, stdout, stderr, cwd, read_buffer, strategy, &flags);
        if (exit_code != ExitCode.Success) {
            return exit_code;
        }
    }

    return ExitCode.Success;
}

fn handlePath(gpa: std.mem.Allocator, path: []const u8, stdout: anytype, stderr: anytype, cwd: std.fs.Dir, read_buffer: []u8, strategy: *SearchStrategy, flags: *HandleFileFlags) !ExitCode {
    const filestat = cwd.statFile(path) catch |e| {
        try stderr.print("Error trying to stat `{s}`: {s}\n", .{ path, @errorName(e) });
        return ExitCode.GenericError;
    };

    switch (filestat.kind) {
        .file => {
            const file = cwd.openFile(path, .{}) catch |e| {
                try stderr.print("Error opening `{s}`: {s}\n", .{ path, @errorName(e) });
                return ExitCode.GenericError;
            };
            defer file.close();

            const result = try handleFile(flags.*, file, path, strategy, stdout, read_buffer);
            if (result) {
                flags.*.first_file = false;
            }
        },
        .directory => {
            var dir = cwd.openDir(path, .{
                .iterate = true,
            }) catch |e| {
                try stderr.print("Error opening `{s}`: {s}\n", .{ path, @errorName(e) });
                return ExitCode.GenericError;
            };

            defer if (cwd.fd != dir.fd) {
                dir.close();
            };

            var walker = try dir.walk(gpa);
            defer walker.deinit();

            const exit_code = try iterateOverDirectory(dir, &walker, stdout, stderr, strategy, read_buffer, flags);
            if (exit_code != ExitCode.Success) {
                return exit_code;
            }
        },
        else => {},
    }

    return ExitCode.Success;
}

fn iterateOverDirectory(dir: std.fs.Dir, walker: *std.fs.Dir.Walker, stdout: anytype, stderr: anytype, strategy: *SearchStrategy, read_buffer: []u8, flags: *HandleFileFlags) !ExitCode {
    while (try walker.next()) |entry| {
        switch (entry.kind) {
            .file => {
                const file = dir.openFile(entry.path, .{}) catch |e| {
                    try stderr.print("Error opening `{s}`: {s}\n", .{ entry.path, @errorName(e) });
                    return ExitCode.GenericError;
                };
                defer file.close();

                const result = try handleFile(flags.*, file, entry.path, strategy, stdout, read_buffer);
                if (result) {
                    flags.*.first_file = false;
                }
            },
            else => {},
        }
    }

    return ExitCode.Success;
}

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

const HandleFileFlags = struct {
    multiple_files: bool,
    first_file: bool,
    line_numbers: bool,
    filenames: bool,
    color: bool,
};

fn handleFile(
    flags: HandleFileFlags,
    file: std.fs.File,
    path: []const u8,
    strategy: *SearchStrategy,
    stdout: anytype,
    buffer: []u8,
) !bool {
    var line_reader = lineReader(buffer, file.reader());
    try line_reader.preread();

    if (isFileBinary(line_reader.cur())) {
        return false;
    }

    var line_number: u32 = 1;
    var printed = false;

    while (try line_reader.next()) |line| : (line_number += 1) {
        printed = try handleLine(line, strategy, line_number, stdout, path, printed, flags) or printed;
    }

    return printed;
}

fn handleLine(
    line: []const u8,
    strategy: *SearchStrategy,
    line_number: u32,
    stdout: anytype,
    filename: []const u8,
    previously_printed: bool,
    flags: HandleFileFlags,
) !bool {
    var printed = previously_printed;
    var start: u32 = 0;
    var printed_line_number = false;
    var found = false;
    const should_print_tail = flags.color;
    while (try strategy.match(!flags.color, line[start..])) |match_result| {
        found = true;
        const slice_start = match_result.start + start;
        const slice_end = match_result.end + start;

        const should_print_filename = flags.filenames and !printed;
        if (should_print_filename) {
            if (!printed and flags.multiple_files and !flags.first_file) {
                try stdout.writeByte('\n');
            }

            if (flags.color) {
                try stdout.writeAll(cli.ANSI.Fg.Magenta);
            }
            try stdout.writeAll(filename);
            if (flags.color) {
                try stdout.writeAll(cli.ANSI.Reset);
            }
            try stdout.writeByte('\n');
        }

        const should_print_line_number = flags.line_numbers and !printed_line_number;
        if (should_print_line_number) {
            if (flags.color) {
                try stdout.writeAll(cli.ANSI.Fg.Green);
            }
            try stdout.print("{d: >4}: ", .{line_number});
            if (flags.color) {
                try stdout.writeAll(cli.ANSI.Reset);
            }
            printed_line_number = true;
        }

        if (slice_start == slice_end) {
            // empty match occured, so we can skip the whole loop and just print the line
            try stdout.print("{s}\n", .{line});
            return true;
        }

        printed = true;
        if (flags.color) {
            try stdout.print("{s}{s}{s}{s}", .{ line[start..slice_start], cli.ANSI.Fg.Red ++ cli.ANSI.Bold, line[slice_start..slice_end], cli.ANSI.Reset });
        } else {
            try stdout.print("{s}\n", .{line});
            break;
        }

        start = slice_end;
    }

    if (should_print_tail and found) {
        try stdout.print("{s}\n", .{line[start..]});
        if (start < line.len) {
            printed = true;
        }
    }

    return printed;
}

fn isFileBinary(buf: []const u8) bool {
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
