const std = @import("std");
const DFA = @import("DFA.zig");
const NFA = @import("NFA.zig");
const DfaBuilder = @import("DfaBuilder.zig");

/// https://en.wikipedia.org/wiki/Powerset_construction
pub fn buildDfaFromNfa(allocator: std.mem.Allocator, nfa: NFA) !DFA {
    var builder = DfaBuilder.init(allocator);
    errdefer builder.deinitResult();

    var set_list = try StateSetList.init(allocator);
    defer set_list.deinit();

    try set_list.ensureEpsilonClosureExists(&builder, nfa, 0);

    var nfa_states_buffer = std.ArrayList(u32).init(allocator);
    defer nfa_states_buffer.deinit();

    while (set_list.getNextSet()) |cur_set_dfa_state_pair| {
        const cur_set = cur_set_dfa_state_pair.set;
        const cur_dfa_state = cur_set_dfa_state_pair.dfa_state;

        try mergeNfaStates(&builder, &set_list, cur_dfa_state, nfa, cur_set, &nfa_states_buffer);
    }

    for (0.., set_list.sets.items) |i, set| {
        const dfa_state = set_list.set_to_dfa_state.items[i].?;
        for (nfa.accepting_states.items) |nfa_accepting_state| {
            std.debug.assert(std.sort.isSorted(u32, set, {}, std.sort.asc(u32)));

            if (std.sort.binarySearch(u32, nfa_accepting_state, set, {}, orderU32) != null) {
                try builder.markStateAccepting(@intCast(dfa_state));
                break;
            }
        }
    }

    return builder.build();
}

fn orderU32(context: void, key: u32, item: u32) std.math.Order {
    _ = context;
    return std.math.order(key, item);
}

fn ascSliceU32(ctx: void, lhs: []const u32, rhs: []const u32) bool {
    _ = ctx;
    return std.mem.lessThan(u32, lhs, rhs);
}

fn mergeNfaStates(builder: *DfaBuilder, set_list: *StateSetList, cur_dfa_state: u32, nfa: NFA, cur_set: []const u32, nfa_states_buffer: *std.ArrayList(u32)) !void {
    std.debug.assert(cur_set.len > 0);

    for (std.math.minInt(u8)..std.math.maxInt(u8) + 1) |key| {
        nfa_states_buffer.clearRetainingCapacity();
        for (cur_set) |nfa_state_index| {
            const cur_state = nfa.states.items[nfa_state_index];
            const transitions_slice = cur_state.transitions.slice();
            for (0..cur_state.transitions.len) |transition_index| {
                const transition = transitions_slice.get(transition_index);
                if (transition.range.matches(@intCast(key))) {
                    if (std.mem.indexOfScalar(u32, nfa_states_buffer.items, transition.dest_index) == null) {
                        try nfa_states_buffer.append(transition.dest_index);
                    }
                }
            }
        }
        if (nfa_states_buffer.items.len > 0) {
            try builder.addTransition(cur_dfa_state, @intCast(key), try set_list.mergeClosures(builder, nfa, nfa_states_buffer.items));
        }
    }
}

const StateSetList = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    sets: std.ArrayListUnmanaged([]const u32) = .{},
    set_to_dfa_state: std.ArrayListUnmanaged(?u32) = .{},
    unvisited_states: std.ArrayListUnmanaged(u32) = .{},

    pub const SetIndex = u32;

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.sets.items) |closure| {
            self.allocator.free(closure);
        }
        self.sets.deinit(self.allocator);
        self.set_to_dfa_state.deinit(self.allocator);
        self.unvisited_states.deinit(self.allocator);
    }

    fn getSet(self: Self, set_index: SetIndex) []const u32 {
        return self.sets.items[set_index];
    }

    pub fn getNextSet(self: *Self) ?struct { set: []const u32, dfa_state: u32 } {
        const result_index = self.unvisited_states.popOrNull();
        if (result_index) |index| {
            return .{
                .set = self.getSet(index),
                .dfa_state = self.set_to_dfa_state.items[index].?,
            };
        } else {
            return null;
        }
    }

    pub fn ensureEpsilonClosureExists(self: *Self, dfa_builder: *DfaBuilder, nfa: NFA, nfa_state: u32) !void {
        const closure = try getEpsilonClosure(self.allocator, nfa, nfa_state);

        var index = self.findSet(closure);
        if (index != null) {
            self.allocator.free(closure);
            return;
        }

        std.debug.assert(closure.len > 0);

        index = self.sets.items.len;
        try self.sets.insert(self.allocator, index.?, closure);
        try self.unvisited_states.append(self.allocator, @intCast(index.?));
        try self.set_to_dfa_state.insert(
            self.allocator,
            index.?,
            try dfa_builder.addState(.{}),
        );
    }

    pub fn mergeClosures(self: *Self, dfa_builder: *DfaBuilder, nfa: NFA, nfa_states: []const u32) !SetIndex {
        std.debug.assert(nfa_states.len > 0);

        var merged_set = std.ArrayList(u32).init(self.allocator);
        defer merged_set.deinit();

        for (nfa_states) |i| {
            const closure = try getEpsilonClosure(self.allocator, nfa, i);
            std.debug.assert(closure.len > 0);

            defer self.allocator.free(closure);
            try merged_set.appendSlice(closure);
        }
        std.debug.assert(merged_set.items.len > 0);

        std.mem.sortUnstable(u32, merged_set.items, {}, std.sort.asc(u32));

        var i: usize = 1;
        while (i < merged_set.items.len) {
            if (merged_set.items[i - 1] == merged_set.items[i]) {
                _ = merged_set.orderedRemove(i);
            } else {
                i += 1;
            }
        }

        var index = self.findSet(merged_set.items);
        if (index == null) {
            index = self.sets.items.len;

            try self.sets.append(self.allocator, try merged_set.toOwnedSlice());
            try self.unvisited_states.append(self.allocator, @intCast(index.?));
            try self.set_to_dfa_state.insert(
                self.allocator,
                index.?,
                try dfa_builder.addState(.{}),
            );
        }

        std.debug.assert(index.? < self.set_to_dfa_state.items.len);
        return self.set_to_dfa_state.items[index.?].?;
    }

    fn findSet(self: Self, needle: []const u32) ?usize {
        for (0.., self.sets.items) |i, set| {
            if (std.mem.eql(u32, set, needle)) {
                return i;
            }
        }
        return null;
    }

    fn getEpsilonClosure(allocator: std.mem.Allocator, nfa: NFA, state: u32) ![]const u32 {
        var visited = std.ArrayList(u32).init(allocator);
        try getEpsilonClosureInternal(nfa, state, &visited);
        return try visited.toOwnedSlice();
    }

    fn getEpsilonClosureInternal(nfa: NFA, state: u32, result: *std.ArrayList(u32)) !void {
        const index = std.sort.lowerBound(
            u32,
            state,
            result.*.items,
            {},
            std.sort.asc(u32),
        );
        const seen = if (index < result.*.items.len) result.*.items[index] == state else false;
        if (seen) {
            return;
        } else {
            try result.insert(index, state);
        }
        for (nfa.states.items[state].epsilon_transitions.items) |dest| {
            try getEpsilonClosureInternal(nfa, dest, result);
        }
    }
};

test "basic NFA to DFA" {
    const NfaBuilder = @import("NfaBuilder.zig");

    const allocator = std.testing.allocator;
    var nfa_builder = NfaBuilder.init(allocator);

    for (0..4) |_| {
        _ = try nfa_builder.addState(.{});
    }

    try nfa_builder.addTransition(0, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('0'),
        .dest_index = 1,
    });
    try nfa_builder.addEpsTransition(0, 2);

    try nfa_builder.addTransition(1, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('1'),
        .dest_index = 1,
    });
    try nfa_builder.addTransition(1, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('1'),
        .dest_index = 3,
    });

    try nfa_builder.addTransition(2, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('0'),
        .dest_index = 3,
    });
    try nfa_builder.addEpsTransition(2, 1);

    try nfa_builder.addTransition(3, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('0'),
        .dest_index = 2,
    });

    try nfa_builder.markStateAccepting(2);
    try nfa_builder.markStateAccepting(3);

    var nfa = nfa_builder.build();
    defer nfa.deinit(allocator);

    var dfa = try buildDfaFromNfa(allocator, nfa);
    defer dfa.deinit(allocator);

    try std.testing.expect(dfa.match(true, "000") != null);
    try std.testing.expect(dfa.match(true, "100") != null);
    try std.testing.expect(dfa.match(true, "01110100") != null);
    try std.testing.expect(dfa.match(true, "1110100") != null);
}

test "NFA loop" {
    const NfaBuilder = @import("NfaBuilder.zig");

    const allocator = std.testing.allocator;
    var nfa_builder = NfaBuilder.init(allocator);

    for (0..3) |_| {
        _ = try nfa_builder.addState(.{});
    }

    try nfa_builder.addTransition(0, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('a'),
        .dest_index = 1,
    });

    try nfa_builder.addEpsTransition(1, 0);

    try nfa_builder.addTransition(1, NFA.Transition{
        .range = NFA.Transition.Range.fromChar('b'),
        .dest_index = 2,
    });

    try nfa_builder.markStateAccepting(2);

    var nfa = nfa_builder.build();
    defer nfa.deinit(allocator);

    var dfa = try buildDfaFromNfa(allocator, nfa);
    defer dfa.deinit(allocator);

    try std.testing.expect(dfa.match(true, "ab") != null);
    try std.testing.expect(dfa.match(true, "aab") != null);
    try std.testing.expect(dfa.match(true, "aaaab") != null);
    try std.testing.expect(dfa.match(true, "aaaaaaaaab") != null);
    try std.testing.expect(!(dfa.match(true, "") != null));
    try std.testing.expect(!(dfa.match(true, "aaaaaaaaaaaaaa") != null));
    try std.testing.expect(!(dfa.match(true, "b") != null));
    try std.testing.expect(!(dfa.match(true, "baa") != null));
}
