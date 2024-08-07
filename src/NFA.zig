const std = @import("std");
const Self = @This();

allocator: std.mem.Allocator,
states: std.ArrayListUnmanaged(State) = .{},
accepting_states: std.ArrayListUnmanaged(u32) = .{},

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    for (self.states.items) |*state| {
        state.deinit(self.allocator);
    }
    self.states.deinit(self.allocator);
    self.accepting_states.deinit(self.allocator);
}

pub fn finalize(self: *Self) void {
    std.mem.sort(u32, self.accepting_states.items, {}, std.sort.asc(u32));
}

pub fn isStateAccepting(self: *const Self, state: u32) bool {
    std.debug.assert(std.sort.isSorted(u32, self.accepting_states.items, {}, std.sort.asc(u32)));
    return std.sort.binarySearch(u32, state, self.accepting_states.items, {}, order(u32)) != null;
}

pub fn walk(self: *const Self, from: u32, key: u8) TransitionIterator {
    const state = self.states.items[from];
    const range = std.sort.equalRange(u8, key, state.transitions.items(.symbol), {}, std.sort.asc(u8));

    return TransitionIterator{ .transitions = state.transitions.items(.dest_index)[range[0]..range[1]] };
}

/// add state and return its index
pub fn addState(self: *Self, state: State) !u32 {
    try self.states.append(self.allocator, state);
    return @intCast(self.states.items.len - 1);
}

pub fn markStateAccepting(self: *Self, i: u32) !void {
    try self.accepting_states.append(self.allocator, i);
}

pub fn addTransition(self: *Self, state: u32, transition: Transition) !void {
    try self.states.items[state].addTransition(self.allocator, transition);
}

pub fn addEpsTransition(self: *Self, state: u32, dest_index: u32) !void {
    try self.states.items[state].addEpsTransition(self.allocator, dest_index);
}

pub const Transition = struct {
    symbol: u8,
    dest_index: u32,
};

pub const TransitionIterator = struct {
    transitions: []const u32 = &[_]u32{},

    pub fn next(self: *TransitionIterator) ?u32 {
        if (self.transitions.len == 0) {
            return null;
        }

        defer self.transitions = self.transitions[1..];
        return self.transitions[0];
    }
};

pub const State = struct {
    transitions: std.MultiArrayList(Transition) = .{},
    epsilon_transitions: std.ArrayListUnmanaged(u32) = .{},

    pub fn deinit(self: *State, allocator: std.mem.Allocator) void {
        self.transitions.deinit(allocator);
        self.epsilon_transitions.deinit(allocator);
    }

    pub fn addTransition(self: *State, allocator: std.mem.Allocator, transition: Transition) !void {
        const i = std.sort.lowerBound(
            u8,
            transition.symbol,
            self.transitions.items(.symbol),
            {},
            std.sort.asc(u8),
        );
        try self.transitions.insert(allocator, i, transition);
    }

    pub fn addEpsTransition(self: *State, allocator: std.mem.Allocator, dest_index: u32) !void {
        try self.epsilon_transitions.append(allocator, dest_index);
    }
};

fn order(comptime T: type) fn (void, T, T) std.math.Order {
    return struct {
        fn impl(context: void, lhs: T, rhs: T) std.math.Order {
            _ = context;
            return std.math.order(lhs, rhs);
        }
    }.impl;
}
