const std = @import("std");
const Self = @This();

states: std.ArrayListUnmanaged(State) = .{},
initial_state: u32 = 0,
states_at_line_start: std.ArrayListUnmanaged(u32) = .{},
states_at_line_end: std.ArrayListUnmanaged(u32) = .{},
accepting_states: std.ArrayListUnmanaged(u32) = .{},

pub const State = struct {
    transitions: std.AutoHashMapUnmanaged(u8, u32) = .{},

    pub fn deinit(self: *State, allocator: std.mem.Allocator) void {
        self.transitions.deinit(allocator);
    }
};

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    for (self.states.items) |*state| {
        state.deinit(allocator);
    }
    self.states.deinit(allocator);
    self.states_at_line_start.deinit(allocator);
    self.states_at_line_end.deinit(allocator);
    self.accepting_states.deinit(allocator);
}

pub fn debugPrint(self: Self) void {
    std.debug.print("initial_state: {d}\n", .{self.initial_state});
    std.debug.print("accepting_states: {any}\n", .{self.accepting_states});

    for (0.., self.states.items) |i, state| {
        std.debug.print("State {d}\n", .{i});
        var iter = state.transitions.iterator();
        while (iter.next()) |entry| {
            std.debug.print("\t{c} -> {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
        }
    }
}

pub fn match(self: Self, line: []const u8) ?struct { start: u32, end: u32 } {
    for (0..line.len) |i| {
        if (self.walk(line[i..], i == 0)) |char_index| {
            return .{ .start = @intCast(i), .end = @intCast(i + char_index) };
        }
    } else {
        if (self.walk("", true)) |char_index| {
            return .{ .start = 0, .end = @intCast(char_index + 1) };
        }
    }

    return null;
}

fn walk(self: Self, input: []const u8, at_line_start: bool) ?u32 {
    var cur_state = self.initial_state;
    for (0..input.len + 1) |i| {
        if (self.isStateAtLineStart(cur_state) and (!at_line_start or i != 0)) {
            return null;
        }

        if (self.isStateAtLineEnd(cur_state) and i < input.len) {
            return null;
        }

        if (self.isStateAccepting(cur_state)) {
            return @intCast(i);
        }

        if (i >= input.len) {
            break;
        }

        const c = input[i];
        const dest_opt = self.getTransition(cur_state, c);
        if (dest_opt) |dest| {
            cur_state = dest;
        } else {
            return null;
        }
    }

    return null;
}

fn isStateAtLineStart(self: Self, state: u32) bool {
    return std.sort.binarySearch(
        u32,
        state,
        self.states_at_line_start.items,
        {},
        order(u32),
    ) != null;
}

fn isStateAtLineEnd(self: Self, state: u32) bool {
    return std.sort.binarySearch(
        u32,
        state,
        self.states_at_line_end.items,
        {},
        order(u32),
    ) != null;
}

fn isStateAccepting(self: Self, state: u32) bool {
    return std.sort.binarySearch(
        u32,
        state,
        self.accepting_states.items,
        {},
        order(u32),
    ) != null;
}

fn order(comptime T: type) fn (void, T, T) std.math.Order {
    return struct {
        pub fn order(context: void, lhs: T, rhs: T) std.math.Order {
            _ = context;
            return std.math.order(lhs, rhs);
        }
    }.order;
}

fn getTransition(self: Self, from: u32, key: u8) ?u32 {
    return self.states.items[from].transitions.get(key);
}
