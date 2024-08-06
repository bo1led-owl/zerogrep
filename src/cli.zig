const std = @import("std");

pub const Args = struct {
    const Self = @This();

    pattern: ?[]const u8,
    filenames: []const []const u8,

    pub fn parse(allocator: std.mem.Allocator) !Args {
        var iter = try std.process.argsWithAllocator(allocator);
        defer iter.deinit();

        _ = iter.skip(); // skip executable name

        const pattern = iter.next();

        var filenames = std.ArrayListUnmanaged([]const u8){};
        while (iter.next()) |filename| {
            try filenames.append(allocator, filename);
        }

        filenames.shrinkAndFree(allocator, filenames.items.len);

        return .{ .pattern = pattern, .filenames = filenames.items };
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        allocator.free(self.filenames);
    }
};
