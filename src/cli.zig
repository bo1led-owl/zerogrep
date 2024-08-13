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
        \\  -D, --data-only: print only matching lines, not filenames or line numbers
    );
}

pub const Args = struct {
    const Self = @This();

    data_only: bool = false,
    print_help: bool = false,
    pattern: []const u8 = "",
    from_stdin: bool = false,
    filenames: []const []const u8 = &[_][]u8{},

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

    pub fn parse(gpa: std.mem.Allocator, arena: std.mem.Allocator) !Result {
        var result = Result.init(gpa, arena);

        var iter = try std.process.argsWithAllocator(gpa);
        defer iter.deinit();

        _ = iter.skip(); // skip executable name
        const pattern = while (iter.next()) |arg| {
            if (std.mem.startsWith(u8, arg, "-")) {
                if (std.mem.eql(u8, "-D", arg) or std.mem.eql(u8, "--data-only", arg)) {
                    result.args.data_only = true;
                } else if (std.mem.eql(u8, "-h", arg) or std.mem.eql(u8, "--help", arg)) {
                    result.args.print_help = true;
                } else {
                    try result.errors.addError("Unknown option: `{s}`, use `--help` to see the guide", .{arg}, {});
                }
            } else {
                break arg;
            }
        } else null;

        var filenames = std.ArrayListUnmanaged([]const u8){};
        while (iter.next()) |filename| {
            try filenames.append(gpa, filename);
        }

        result.args.filenames = try filenames.toOwnedSlice(gpa);

        if (pattern) |pat| {
            result.args.pattern = pat;
        } else {
            try result.errors.addError("No search pattern provided", .{}, {});
        }

        if (filenames.items.len == 0) {
            if (!std.posix.isatty(std.io.getStdIn().handle)) {
                result.args.from_stdin = true;
                result.args.data_only = true;
            } else {
                try result.errors.addError("No input provided", .{}, {});
            }
        }

        return result;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.filenames);
    }
};
