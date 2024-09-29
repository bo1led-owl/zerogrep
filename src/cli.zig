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
        \\  -p, --pretty: print colored output with filenames and line numbers
        \\  -d, --data-only: print only matching lines, not filenames or line numbers
        \\  -r, --recursive: search recursively in all subdirectories
    );
}

pub const Args = struct {
    const Self = @This();

    data_only: bool = false,
    pretty: bool = true,
    print_help: bool = false,
    pattern: []const u8 = "",
    recursive: bool = false,
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

        if (stdin_is_tty or !stdout_is_tty) {
            result.args.data_only = true;
            result.args.pretty = false;
        }

        var iter = try std.process.argsWithAllocator(gpa);
        defer iter.deinit();

        _ = iter.skip(); // skip executable name
        const pattern = while (iter.next()) |arg| {
            if (std.mem.startsWith(u8, arg, "-")) {
                if (std.mem.eql(u8, "-d", arg) or std.mem.eql(u8, "--data-only", arg)) {
                    result.args.data_only = true;
                    result.args.pretty = false;
                } else if (std.mem.eql(u8, "-p", arg) or std.mem.eql(u8, "--pretty", arg)) {
                    result.args.pretty = true;
                    result.args.data_only = false;
                } else if (std.mem.eql(u8, "-h", arg) or std.mem.eql(u8, "--help", arg)) {
                    result.args.print_help = true;
                } else if (std.mem.eql(u8, "-r", arg) or std.mem.eql(u8, "--recursive", arg)) {
                    result.args.recursive = true;
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

        return result;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.paths);
    }
};
