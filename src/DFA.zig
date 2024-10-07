const std = @import("std");
const Self = @This();
const Builder = @import("DfaBuilder.zig");
const MatchResult = @import("Regex.zig").MatchResult;

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

pub fn match(self: Self, lazy: bool, line: []const u8) ?MatchResult {
    for (0..line.len) |i| {
        if (self.walk(lazy, line[i..], i == 0)) |char_index| {
            return .{ .start = @intCast(i), .end = @intCast(i + char_index) };
        }
    } else {
        if (self.walk(lazy, "", true)) |char_index| {
            return .{ .start = 0, .end = @intCast(char_index) };
        }
    }

    return null;
}

fn walk(self: Self, lazy: bool, input: []const u8, at_line_start: bool) ?u32 {
    var result: ?u32 = null;

    var cur_state = self.initial_state;
    for (0..input.len + 1) |i| {
        if (self.isStateAtLineStart(cur_state) and (!at_line_start or i != 0)) {
            break;
        }

        if (self.isStateAtLineEnd(cur_state) and i < input.len) {
            break;
        }

        if (self.isStateAccepting(cur_state)) {
            if (lazy) {
                return @intCast(i);
            }

            result = @intCast(i);
        }

        if (i >= input.len) {
            break;
        }

        const c = input[i];
        const dest_opt = self.getTransition(cur_state, c);
        if (dest_opt) |dest| {
            cur_state = dest;
        } else {
            break;
        }
    }

    return result;
}

pub fn isStateAtLineStart(self: Self, state: u32) bool {
    return std.sort.binarySearch(
        u32,
        state,
        self.states_at_line_start.items,
        {},
        order(u32),
    ) != null;
}

pub fn isStateAtLineEnd(self: Self, state: u32) bool {
    return std.sort.binarySearch(
        u32,
        state,
        self.states_at_line_end.items,
        {},
        order(u32),
    ) != null;
}

pub fn isStateAccepting(self: Self, state: u32) bool {
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

test "basic" {
    //      a        b
    // (0) ---> (1) ---> ((2))
    //

    const allocator = std.testing.allocator;
    var builder = Builder.init(allocator);

    for (0..3) |_| {
        _ = try builder.addState(.{});
    }

    try builder.markStateAccepting(2);

    try builder.addTransition(0, 'a', 1);
    try builder.addTransition(1, 'b', 2);

    var dfa = builder.build();
    defer dfa.deinit(allocator);

    try std.testing.expect((dfa.match(true, "ab") != null));
    try std.testing.expect((dfa.match(true, "cab") != null));
    try std.testing.expect((dfa.match(true, "abc") != null));
    try std.testing.expect(!(dfa.match(true, "a") != null));
    try std.testing.expect(!(dfa.match(true, "b") != null));
    try std.testing.expect(!(dfa.match(true, "") != null));
    try std.testing.expect(!(dfa.match(true, "ac") != null));
}

test "branching" {
    //      a
    //    ----> (1) --\
    //  /              \ a
    //  |   b        b  \
    // (0) ---> (2) -----> ((4))
    //  |               /
    //  \   c          / c
    //   -----> (3) --/

    const allocator = std.testing.allocator;
    var builder = Builder.init(allocator);

    for (0..5) |_| {
        _ = try builder.addState(.{});
    }
    try builder.markStateAccepting(4);

    try builder.addTransition(0, 'a', 1);
    try builder.addTransition(0, 'b', 2);
    try builder.addTransition(0, 'c', 3);

    try builder.addTransition(1, 'a', 4);
    try builder.addTransition(2, 'b', 4);
    try builder.addTransition(3, 'c', 4);

    var dfa = builder.build();
    defer dfa.deinit(allocator);

    try std.testing.expect((dfa.match(true, "aa") != null));
    try std.testing.expect((dfa.match(true, "aab") != null));
    try std.testing.expect((dfa.match(true, "bb") != null));
    try std.testing.expect((dfa.match(true, "cc") != null));
    try std.testing.expect(!(dfa.match(true, "") != null));
    try std.testing.expect(!(dfa.match(true, "a") != null));
    try std.testing.expect(!(dfa.match(true, "ab") != null));
    try std.testing.expect(!(dfa.match(true, "cb") != null));
    try std.testing.expect(!(dfa.match(true, "cf") != null));
    try std.testing.expect(!(dfa.match(true, "b") != null));
    try std.testing.expect(!(dfa.match(true, "c") != null));
    try std.testing.expect(!(dfa.match(true, "d") != null));
    try std.testing.expect(!(dfa.match(true, "foo") != null));
}

test "loop" {
    //       b
    //  (0) ---> ((1))
    //   ^
    //  / \ a
    //  \/

    const allocator = std.testing.allocator;
    var builder = Builder.init(allocator);

    for (0..2) |_| {
        _ = try builder.addState(.{});
    }
    try builder.markStateAccepting(1);

    try builder.addTransition(0, 'a', 0);
    try builder.addTransition(0, 'b', 1);

    var dfa = builder.build();
    defer dfa.deinit(allocator);

    try std.testing.expect(!(dfa.match(true, "") != null));
    try std.testing.expect((dfa.match(true, "ab") != null));
    try std.testing.expect((dfa.match(true, "aab") != null));
    try std.testing.expect((dfa.match(true, "b") != null));
    try std.testing.expect(!(dfa.match(true, "foo") != null));
    try std.testing.expect(!(dfa.match(true, "a") != null));
}
