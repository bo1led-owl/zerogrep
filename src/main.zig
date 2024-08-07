const std = @import("std");
const cli = @import("cli.zig");
const regex = @import("regex.zig");
const NFA = @import("NFA.zig");

const Errors = @import("Errors.zig");

const KiB = 1024;
const MiB = 1024 * KiB;
const GiB = 1024 * MiB;

const SearchStrategy = union(enum) {
    const Self = @This();

    Substring: []const u8,
    Regex: struct {
        gpa: std.mem.Allocator,
        nfa: NFA,
    },

    pub fn match(self: *const Self, line: []const u8) !bool {
        switch (self.*) {
            .Substring => |s| return std.mem.indexOf(u8, line, s) != null,
            .Regex => |r| {
                for (0..line.len) |i| {
                    if (try walkNFA(r.gpa, r.nfa, line[i..])) {
                        return true;
                    }
                }
                return false;
            },
        }
    }

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            .Substring => {},
            .Regex => |*r| {
                r.nfa.deinit();
            },
        }
    }
};

pub fn main() void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        std.debug.assert(gpa.deinit() != .leak);
    }

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const stderr_file = std.io.getStdErr().writer();
    var bw_stderr = std.io.bufferedWriter(stderr_file);
    const stderr = bw_stderr.writer();

    var errors = run(allocator, arena.allocator()) catch |e| {
        stderr.print("Error: {s}\n", .{@errorName(e)}) catch {
            @panic("Couldn't write to stderr");
        };
        bw_stderr.flush() catch {
            @panic("Couldn't write to stderr");
        };
        return;
    };
    defer errors.deinit();

    for (errors.errors.items) |err| {
        stderr.print("{s}\n", .{err}) catch {
            @panic("Couldn't write to stderr");
        };
    }
    bw_stderr.flush() catch {
        @panic("Couldn't write to stderr");
    };
}

fn run(gpa: std.mem.Allocator, arena: std.mem.Allocator) !Errors {
    var errors = Errors.init(gpa, arena);

    const stdout_file = std.io.getStdOut().writer();
    var bw_stdout = std.io.bufferedWriter(stdout_file);
    const stdout = bw_stdout.writer();

    var args = try cli.Args.parse(gpa, &errors);
    defer args.deinit(gpa);
    if (errors.errors.items.len != 0) {
        try errors.addError("Errors occured, use `--help` to see the guide", .{});
        return errors;
    }

    if (args.print_help or args.pattern == null) {
        try cli.printHelp(stdout);
        try bw_stdout.flush();
        return errors;
    }

    var input_files = std.ArrayListUnmanaged(std.fs.File){};
    defer input_files.deinit(gpa);
    var got_error = false;
    const cwd = std.fs.cwd();
    for (args.filenames) |filename| {
        const file = cwd.openFile(filename, .{}) catch |e| {
            got_error = true;
            try errors.addError("Error opening `{s}`: {any}\n", .{ filename, e });
            continue;
        };

        try input_files.append(gpa, file);
    }

    if (got_error) {
        return errors;
    }

    if (input_files.items.len == 0) {
        const stdin_file = std.io.getStdIn();

        if (!std.posix.isatty(stdin_file.handle)) {
            try input_files.append(gpa, stdin_file);
        } else {
            try cli.printHelp(stdout);
            try bw_stdout.flush();
            return errors;
        }
    }

    const pattern = args.pattern.?;

    var strategy =
        if (try regex.isStringLiteral(pattern))
        SearchStrategy{
            .Substring = pattern,
        }
    else blk: {
        const initial_error_count = errors.errors.items.len;

        var nfa = try regex.buildNFA(gpa, pattern, &errors);

        if (errors.errors.items.len > initial_error_count) {
            return errors;
        }

        nfa.finalize();

        break :blk SearchStrategy{ .Regex = .{
            .gpa = gpa,
            .nfa = nfa,
        } };
    };
    defer strategy.deinit();

    const buffer = try gpa.alloc(u8, 1 * KiB);
    defer gpa.free(buffer);

    const multiple_files = input_files.items.len > 0;
    for (0.., args.filenames, input_files.items) |i, filename, file| {
        var buffered_reader = std.io.bufferedReader(file.reader());
        var reader = buffered_reader.reader();

        var line_number: u32 = 1;
        var printed_filename = false;
        while (try reader.readUntilDelimiterOrEof(buffer, '\n')) |line| : (line_number += 1) {
            if (!try strategy.match(line)) {
                continue;
            }

            if (!args.data_only) {
                const should_print_filename = multiple_files and !printed_filename;
                const should_print_line_number = multiple_files;
                
                if (should_print_filename) {
                    try stdout.print("{s}{s}{s}\n", .{ cli.ANSI.Fg.Magenta, filename, cli.ANSI.Reset });
                    printed_filename = true;
                }

                if (should_print_line_number) {
                    try stdout.print("{s}{d}:{s} ", .{ cli.ANSI.Fg.Green, line_number, cli.ANSI.Reset });
                }
            }

            try stdout.print("{s}\n", .{line});
        }

        const last_file = i == input_files.items.len - 1;
        if (!args.data_only and !last_file) {
            try stdout.writeByte('\n');
        }
    }
    try bw_stdout.flush();

    return errors;
}

fn walkNFA(allocator: std.mem.Allocator, nfa: NFA, input: []const u8) !bool {
    var stack = std.ArrayListUnmanaged(struct {
        state_index: u32,
        input_start: u32,
        iter: NFA.TransitionIterator,
    }){};
    defer stack.deinit(allocator);

    try stack.append(allocator, .{
        .state_index = 0,
        .input_start = 0,
        .iter = if (input.len > 0) nfa.walk(0, input[0]) else .{},
    });

    while (stack.items.len > 0) {
        const state_index = stack.getLast().state_index;
        const char_index = stack.getLast().input_start;
        var iter = &stack.items[stack.items.len - 1].iter;

        if (nfa.isStateAccepting(state_index)) {
            return true;
        }

        // TODO: epsilon transitions
        // const state = automata.states.items[state_index];

        if (char_index >= input.len) {
            _ = stack.popOrNull();
            continue;
        }

        if (iter.next()) |dest| {
            const new_iter = if (char_index + 1 < input.len) nfa.walk(dest, input[char_index + 1]) else NFA.TransitionIterator{};

            try stack.append(allocator, .{
                .state_index = dest,
                .input_start = char_index + 1,
                .iter = new_iter,
            });
        } else {
            _ = stack.popOrNull();
        }
    }

    return false;
}
