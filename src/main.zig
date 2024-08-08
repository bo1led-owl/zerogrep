const std = @import("std");
const cli = @import("cli.zig");
const Regex = @import("Regex.zig");
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

    if (errors.count() != 0) {
        try errors.addError("Errors occured, use `--help` to see the guide", .{});
        return errors;
    }

    if (args.print_help) {
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

    var strategy =
        if (try Regex.isStringLiteral(args.pattern))
    literal_blk: {
        break :literal_blk SearchStrategy{
            .Substring = args.pattern,
        };
    } else regex_blk: {
        const regex = Regex.init(gpa, &errors);
        const nfa_build_result = try regex.buildNFA(args.pattern);

        if (nfa_build_result.errors_occured) {
            return errors;
        }

        const nfa = nfa_build_result.automata;

        // std.debug.print("{any}\n", .{nfa});
        // for (0.., nfa.states.items) |i, state| {
        //     std.debug.print("{d} {any}\n", .{ i, state });
        //     for (0..state.transitions.len) |j| {
        //         const transition = state.transitions.get(j);

        //         std.debug.print("\t{c} -> {d}\n", .{ transition.symbol, transition.dest_index });
        //     }
        // }

        break :regex_blk SearchStrategy{ .Regex = .{
            .gpa = gpa,
            .nfa = nfa,
        } };
    };
    defer strategy.deinit();

    const buffer = try gpa.alloc(u8, 1 * KiB);
    defer gpa.free(buffer);

    if (!args.stdin) {
        const multiple_files = input_files.items.len > 0;
        for (0.., args.filenames, input_files.items) |i, filename, file| {
            var buffered_reader = std.io.bufferedReader(file.reader());
            var reader = buffered_reader.reader();

            const last_file = i == input_files.items.len - 1;
            const flags = HandleFileFlags{
                .data_only = args.data_only,
                .multiple_files = multiple_files,
                .last_file = last_file,
            };
            try handleFile(filename, &reader, buffer, stdout, strategy, flags);
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
        try handleFile("stdin", &reader, buffer, stdout, strategy, flags);
    }
    try bw_stdout.flush();

    return errors;
}

const HandleFileFlags = struct {
    data_only: bool,
    multiple_files: bool,
    last_file: bool,
};

fn handleFile(filename: []const u8, reader: anytype, buffer: []u8, stdout: anytype, strategy: SearchStrategy, flags: HandleFileFlags) !void {
    var line_number: u32 = 1;
    var printed_filename = false;
    var printed = false;
    while (try reader.readUntilDelimiterOrEof(buffer, '\n')) |line| : (line_number += 1) {
        if (!try strategy.match(line)) {
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

        if (nfa.accepting_state == state_index) {
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
