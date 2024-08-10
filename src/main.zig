const std = @import("std");
const cli = @import("cli.zig");
const Regex = @import("Regex.zig");
const NFA = @import("NFA.zig");

const Errors = @import("Errors.zig");

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

    // var timer = try std.time.Timer.start();

    var regex = Regex.init(gpa, &errors, args.pattern);
    const nfa_build_result = try regex.buildNFA();

    // const nfa_build_time: f64 = @floatFromInt(timer.read());
    // std.debug.print("NFA is built in {d:.3}us\n", .{
    //     nfa_build_time / std.time.ns_per_us,
    // });

    var nfa = nfa_build_result.automata;
    defer nfa.deinit();
    if (nfa_build_result.errors_occured) {
        return errors;
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
                try errors.addError("Error opening `{s}`: {s}\n", .{ filename, @errorName(e) });
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

    return errors;
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
