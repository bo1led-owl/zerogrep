const std = @import("std");
const Errors = @import("Errors.zig");

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
        \\zg [OPTION]... <PATTERN> [FILE]...
        \\Options:
        \\  -h, --help: print this message
        \\  -D, --data-only: print only matching strings, not filenames or line numbers
    );
}

pub const Args = struct {
    const Self = @This();

    data_only: bool = false,
    print_help: bool = false,
    pattern: ?[]const u8 = null,
    filenames: []const []const u8 = &[_][]u8{},

    pub fn parse(allocator: std.mem.Allocator, errors: *Errors) !Args {
        var self = Self{};

        var iter = try std.process.argsWithAllocator(allocator);
        defer iter.deinit();

        _ = iter.skip(); // skip executable name
        self.pattern = while (iter.next()) |arg| {
            if (dashCount(arg) > 0) {
                try self.parseOption(arg, errors);
            } else {
                break arg;
            }
        } else null;

        var filenames = std.ArrayListUnmanaged([]const u8){};
        while (iter.next()) |filename| {
            try filenames.append(allocator, filename);
        }

        filenames.shrinkAndFree(allocator, filenames.items.len);
        self.filenames = filenames.items;

        return self;
    }

    fn dashCount(arg: []const u8) u8 {
        var res: u8 = 0;
        for (arg) |c| {
            if (c != '-') {
                break;
            }
            res += 1;
        }
        return res;
    }

    fn parseOption(self: *Self, option: []const u8, errors: *Errors) !void {
        if (std.mem.eql(u8, "-D", option) or std.mem.eql(u8, "--data-only", option)) {
            self.data_only = true;
        } else if (std.mem.eql(u8, "-h", option) or std.mem.eql(u8, "--help", option)) {
            self.print_help = true;
        } else {
            try errors.addError("Unknown option: `{s}`, use `--help` to see the guide", .{option});
        }
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.filenames);
    }
};
