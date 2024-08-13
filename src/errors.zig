pub fn Errors(comptime PayloadT: type) type {
    return struct {
        const Self = @This();
        const std = @import("std");

        const Error = struct { message: []const u8, payload: PayloadT };

        arena: std.mem.Allocator,
        errors: std.ArrayList(Error),

        pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator) Self {
            return .{
                .arena = arena,
                .errors = std.ArrayList(Error).init(gpa),
            };
        }

        pub fn deinit(self: *Self) void {
            self.errors.deinit();
        }

        pub fn items(self: Self) []const Error {
            return self.errors.items;
        }

        pub fn addError(self: *Self, comptime fmt: []const u8, args: anytype, payload: PayloadT) !void {
            try self.errors.append(.{
                .message = try std.fmt.allocPrint(self.arena, fmt, args),
                .payload = payload,
            });
        }

        pub fn count(self: Self) usize {
            return self.errors.items.len;
        }
    };
}
