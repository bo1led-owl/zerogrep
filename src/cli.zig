const std = @import("std");
const Errors = @import("errors.zig").Errors;

pub const ANSI = struct {
    fn genANSILiteral(comptime code: u8) []const u8 {
        return std.fmt.comptimePrint("\x1B[{d}m", .{code});
    }

    pub const Reset = genANSILiteral(0);
    pub const Bold = genANSILiteral(1);

    pub const Fg = struct {
        pub const Black = genANSILiteral(30);
        pub const Red = genANSILiteral(31);
        pub const Green = genANSILiteral(32);
        pub const Yellow = genANSILiteral(33);
        pub const Blue = genANSILiteral(34);
        pub const Magenta = genANSILiteral(35);
        pub const Cyan = genANSILiteral(36);
        pub const White = genANSILiteral(37);
        pub const BrightBlack = genANSILiteral(100);
        pub const BrightRed = genANSILiteral(101);
        pub const BrightGreen = genANSILiteral(102);
        pub const BrightYellow = genANSILiteral(103);
        pub const BrightBlue = genANSILiteral(104);
        pub const BrightMagenta = genANSILiteral(105);
        pub const BrightCyan = genANSILiteral(106);
        pub const BrightWhite = genANSILiteral(107);
    };
};

pub fn printHelp(writer: anytype) !void {
    try writer.writeAll(
        \\Usage:
        \\  zg [OPTION]... <PATTERN> [FILE]...
        \\
        \\Options:
        \\  -h, --help: print this message
        \\  -f, --filenames: print filenames
        \\  -F, --no-filenames: do not print filenames
        \\  -n, --line-numbers: print line numbers
        \\  -N, --no-line-numbers: do not print line numbers
        \\  --color=[auto, on, off]: specify when to highlight the output
    );
}

pub const Args = struct {
    const Self = @This();

    print_help: bool = false,
    color: bool = true,
    filenames: bool = true,
    line_numbers: bool = true,

    pattern: []const u8 = "",
    paths: []const []const u8 = &[_][]u8{},

    pub const Result = struct {
        args: Args,
        errors: Errors(void),

        pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator) Result {
            return .{
                .args = .{},
                .errors = Errors(void).init(gpa, arena),
            };
        }
    };

    pub fn parse(gpa: std.mem.Allocator, arena: std.mem.Allocator, stdin_is_tty: bool, stdout_is_tty: bool) !Result {
        var result = Result.init(gpa, arena);

        if (!stdout_is_tty) {
            result.args.filenames = false;
            result.args.line_numbers = false;
            result.args.color = false;
        }
        if (!stdin_is_tty) {
            result.args.filenames = false;
        }

        var iter = try std.process.argsWithAllocator(gpa);
        defer iter.deinit();

        _ = iter.skip(); // skip executable name
        const pattern = while (iter.next()) |arg| {
            if (std.mem.startsWith(u8, arg, "-")) {
                if (std.mem.eql(u8, "-f", arg) or std.mem.eql(u8, "--filenames", arg)) {
                    result.args.filenames = true;
                } else if (std.mem.eql(u8, "-F", arg) or std.mem.eql(u8, "--no-filenames", arg)) {
                    result.args.filenames = false;
                } else if (std.mem.eql(u8, "-n", arg) or std.mem.eql(u8, "--line-numbers", arg)) {
                    result.args.line_numbers = true;
                } else if (std.mem.eql(u8, "-N", arg) or std.mem.eql(u8, "--no-line-numbers", arg)) {
                    result.args.line_numbers = true;
                } else if (std.mem.startsWith(u8, "--color=", arg)) {
                    const value = arg["--color=".len..];
                    if (std.mem.eql(u8, value, "auto")) {
                        result.args.color = stdout_is_tty;
                    } else if (std.mem.eql(u8, value, "on")) {
                        result.args.color = true;
                    } else if (std.mem.eql(u8, value, "off")) {
                        result.args.color = false;
                    } else {
                        try result.errors.addError("Unknown value for `--color`: `{s}`. Possible values are `auto`, `on` and `off`", .{value}, {});
                    }
                } else if (std.mem.eql(u8, "-h", arg) or std.mem.eql(u8, "--help", arg)) {
                    result.args.print_help = true;
                } else {
                    try result.errors.addError("Unknown option: `{s}`, use `--help` to see the guide", .{arg}, {});
                }
            } else {
                break arg;
            }
        } else null;

        var paths = std.ArrayListUnmanaged([]const u8){};
        while (iter.next()) |filename| {
            try paths.append(gpa, filename);
        }

        result.args.paths = try paths.toOwnedSlice(gpa);

        if (pattern) |pat| {
            result.args.pattern = pat;
        } else {
            try result.errors.addError("No search pattern provided", .{}, {});
        }

        if (result.args.paths.len == 0 and stdin_is_tty) {
            try result.errors.addError("No input provided", .{}, {});
        }

        return result;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.paths);
    }
};
