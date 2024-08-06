const Self = @This();
const std = @import("std");

arena: std.mem.Allocator,
errors: std.ArrayList([]const u8),

pub fn init(gpa: std.mem.Allocator, arena: std.mem.Allocator) Self {
    return .{
        .arena = arena,
        .errors = std.ArrayList([]const u8).init(gpa),
    };
}

pub fn deinit(self: *Self) void {
    self.errors.deinit();
}

pub fn addError(self: *Self, comptime fmt: []const u8, args: anytype) !void {
    try self.errors.append(try std.fmt.allocPrint(self.arena, fmt, args));
}
