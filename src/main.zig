const std = @import("std");
const cli = @import("cli.zig");
const regex = @import("regex.zig");
const nfa_mod = @import("nfa.zig");

const Errors = @import("Errors.zig");
const NFA = nfa_mod.NFA;

const KiB = 1024;
const MiB = 1024 * KiB;
const GiB = 1024 * MiB;

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

    var args = try cli.Args.parse(gpa);
    defer args.deinit(gpa);

    if (args.pattern == null) {
        try printHelp(stdout);
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
            try printHelp(stdout);
            try bw_stdout.flush();
            return errors;
        }
    }

    const pattern = args.pattern.?;

    if (try regex.isStringLiteral(pattern)) {
        const buffer = try gpa.alloc(u8, 1 * KiB);
        defer gpa.free(buffer);

        for (input_files.items) |file| {
            var buffered_reader = std.io.bufferedReader(file.reader());
            var reader = buffered_reader.reader();

            while (try reader.readUntilDelimiterOrEof(buffer, '\n')) |line| {
                if (std.mem.indexOf(u8, line, pattern) != null) {
                    try stdout.print("{s}\n", .{line});
                }
            }
        }
        try bw_stdout.flush();
    } else {
        const initial_error_count = errors.errors.items.len;

        var nfa = try regex.buildNFA(gpa, pattern, &errors);
        defer nfa.deinit();

        if (errors.errors.items.len > initial_error_count) {
            return errors;
        }

        nfa.finalize();

        const buffer = try gpa.alloc(u8, 1 * KiB);
        defer gpa.free(buffer);
        for (input_files.items) |file| {
            var buffered_reader = std.io.bufferedReader(file.reader());
            var reader = buffered_reader.reader();

            while (try reader.readUntilDelimiterOrEof(buffer, '\n')) |line| {
                for (0..line.len) |i| {
                    if (try walkNFA(gpa, nfa, line[i..])) {
                        try stdout.print("{s}\n", .{line});
                        break;
                    }
                }
            }
        }
        try bw_stdout.flush();
    }

    return errors;
}

fn printHelp(writer: anytype) !void {
    try writer.writeAll(
        \\Usage:
        \\zgrep [OPTION]... <PATTERN> [FILE]... 
    );
}

fn walkNFA(allocator: std.mem.Allocator, automata: NFA, input: []const u8) !bool {
    var stack = std.ArrayListUnmanaged(struct {
        state_index: u32,
        input_start: u32,
        iter: nfa_mod.TransitionIterator,
    }){};
    defer stack.deinit(allocator);

    try stack.append(allocator, .{
        .state_index = 0,
        .input_start = 0,
        .iter = if (input.len > 0) automata.walk(0, input[0]) else .{},
    });

    while (stack.items.len > 0) {
        const state_index = stack.getLast().state_index;
        const char_index = stack.getLast().input_start;
        var iter = &stack.items[stack.items.len - 1].iter;

        if (automata.isStateAccepting(state_index)) {
            return true;
        }

        // TODO: epsilon transitions
        // const state = automata.states.items[state_index];

        if (char_index >= input.len) {
            _ = stack.popOrNull();
            continue;
        }

        if (iter.next()) |dest| {
            const new_iter = if (char_index + 1 < input.len) automata.walk(dest, input[char_index + 1]) else nfa_mod.TransitionIterator{};

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
