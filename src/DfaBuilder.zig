const std = @import("std");
const DFA = @import("DFA.zig");
const NFA = @import("NFA.zig");

const Self = @This();

const DfaBuilder = Self;

allocator: std.mem.Allocator,
result: DFA,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .result = undefined,
    };
}

fn build(self: *Self) DFA {
    defer self.result = undefined;
    return self.result;
}

const SetToStateMapCtx = struct {
    pub fn hash(ctx: SetToStateMapCtx, key: []const u32) u64 {
        _ = ctx;
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, key, std.hash.Strategy.Deep);
        return hasher.final();
    }

    pub fn eql(ctx: SetToStateMapCtx, lhs: []const u32, rhs: []const u32) bool {
        _ = ctx;
        return std.mem.eql(u32, lhs, rhs);
    }
};

const SetToStateMap = std.HashMap(
    []const u32,
    u32,
    SetToStateMapCtx,
    std.hash_map.default_max_load_percentage,
);

const ScanlineEvent = struct {
    key: u8,
    delta: i8,
    dest_index: u32,

    pub fn lessThan(ctx: void, lhs: ScanlineEvent, rhs: ScanlineEvent) bool {
        _ = ctx;
        if (lhs.key != rhs.key) {
            return lhs.key < rhs.key;
        }
        return lhs.delta > rhs.delta;
    }
};

const Scanline = std.ArrayList(ScanlineEvent);

/// https://en.wikipedia.org/wiki/Powerset_construction
pub fn buildFromNFA(self: *Self, nfa: NFA) std.mem.Allocator.Error!DFA {
    self.result = DFA{};
    errdefer self.result.deinit(self.allocator);

    var set_list = try StateSetList.init(self.allocator);
    defer set_list.deinit();

    try set_list.getOrInsertEpsilonClosure(self, nfa, 0);

    {
        var scanline = Scanline.init(self.allocator);
        defer scanline.deinit();

        const nfa_state_count = try self.allocator.alloc(u32, nfa.states.items.len);
        @memset(nfa_state_count, 0);
        defer self.allocator.free(nfa_state_count);

        var nfa_states_set = std.ArrayList(u32).init(self.allocator);
        defer nfa_states_set.deinit();

        while (set_list.getNextSet()) |cur_set_dfa_state_pair| {
            const cur_set = cur_set_dfa_state_pair.set;
            const cur_dfa_state = cur_set_dfa_state_pair.dfa_state;

            scanline.clearRetainingCapacity();
            for (cur_set) |state| {
                for (0..nfa.states.items[state].transitions.len) |transition_i| {
                    const transition = nfa.states.items[state].transitions.get(transition_i);
                    try scanline.append(ScanlineEvent{
                        .key = transition.range.start,
                        .delta = 1,
                        .dest_index = transition.dest_index,
                    });
                    try scanline.append(ScanlineEvent{
                        .key = transition.range.end + 1,
                        .delta = -1,
                        .dest_index = transition.dest_index,
                    });
                }
            }

            std.mem.sort(ScanlineEvent, scanline.items, {}, ScanlineEvent.lessThan);

            std.debug.assert(std.mem.allEqual(u32, nfa_state_count, 0));
            if (scanline.items.len > 0) {
                var prev_key = scanline.items[0].key;
                var prev_prev_key = scanline.items[0].key;
                for (scanline.items) |event| {
                    defer {
                        prev_key = event.key;
                        prev_prev_key = prev_key;
                    }

                    if (event.key != prev_key) {
                        nfa_states_set.clearRetainingCapacity();
                        for (0.., nfa_state_count) |i, count| {
                            if (count > 0) {
                                try nfa_states_set.append(@intCast(i));
                            }
                        }

                        const set_index = try set_list.mergeClosures(self, nfa, nfa_states_set.items);
                        const dest_index = set_list.set_to_dfa_state.items[set_index].?;

                        try self.addTransition(
                            cur_dfa_state,
                            .{
                                .range = DFA.Transition.Range
                                    .fromRange(prev_prev_key, prev_key),
                                .dest_index = dest_index,
                            },
                        );
                    }

                    if (event.delta > 0) {
                        nfa_state_count[event.dest_index] += 1;
                    } else {
                        nfa_state_count[event.dest_index] -= 1;
                    }
                }
            }
            std.debug.assert(std.mem.allEqual(u32, nfa_state_count, 0));
        }
    }

    for (0.., set_list.sets.items) |i, set| {
        for (nfa.accepting_states.items) |nfa_accepting_state| {
            if (std.mem.indexOfScalar(u32, set, nfa_accepting_state) != null) {
                try self.markStateAccepting(@intCast(i));
            }
        }
    }
    return self.build();
}

/// https://en.wikipedia.org/wiki/DFA_minimization
pub fn minimizeDFA(self: *Self, original_dfa: DFA) std.mem.Allocator.Error!DFA {
    self.result = DFA{};
    errdefer self.result.deinit(self.allocator);

    _ = original_dfa;

    return self.build();
}

const StateSetList = struct {
    allocator: std.mem.Allocator,
    sets: std.ArrayListUnmanaged([]const u32) = .{},
    set_to_dfa_state: std.ArrayListUnmanaged(?u32) = .{},
    unvisited_states: std.ArrayListUnmanaged(u32) = .{},

    pub const SetIndex = u32;

    pub fn init(allocator: std.mem.Allocator) !StateSetList {
        return StateSetList{
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *StateSetList) void {
        for (self.sets.items) |closure| {
            self.allocator.free(closure);
        }
        self.sets.deinit(self.allocator);
        self.set_to_dfa_state.deinit(self.allocator);
        self.unvisited_states.deinit(self.allocator);
    }

    fn getSet(self: StateSetList, set_index: SetIndex) []const u32 {
        return self.sets.items[set_index];
    }

    pub fn getNextSet(self: *StateSetList) ?struct { set: []const u32, dfa_state: u32 } {
        const result_index = self.unvisited_states.popOrNull();
        if (result_index == null) {
            return null;
        }

        return .{
            .set = self.getSet(result_index.?),
            .dfa_state = self.set_to_dfa_state.items[result_index.?].?,
        };
    }

    pub fn getOrInsertEpsilonClosure(self: *StateSetList, dfa_builder: *DfaBuilder, nfa: NFA, nfa_state: u32) !void {
        const closure = try getEpsilonClosure(self.allocator, nfa, nfa_state);

        const index = self.setLowerBound(closure);
        const set_is_present = index < self.sets.items.len and std.mem.eql(u32, closure, self.sets.items[index]);
        if (set_is_present) {
            self.allocator.free(closure);
            return;
        }

        try self.sets.insert(self.allocator, index, closure);
        try self.unvisited_states.append(self.allocator, index);
        try self.set_to_dfa_state.insert(
            self.allocator,
            index,
            try dfa_builder.addState(.{}),
        );
    }

    pub fn mergeClosures(self: *StateSetList, dfa_builder: *DfaBuilder, nfa: NFA, nfa_states: []const u32) !SetIndex {
        var merged_set = std.ArrayList(u32).init(self.allocator);
        defer merged_set.deinit();

        for (nfa_states) |i| {
            const closure = try getEpsilonClosure(self.allocator, nfa, i);
            defer self.allocator.free(closure);
            try merged_set.appendSlice(closure);
        }
        std.mem.sortUnstable(u32, merged_set.items, {}, std.sort.asc(u32));
        var i: usize = 1;
        while (i < merged_set.items.len) {
            if (merged_set.items[i - 1] == merged_set.items[i]) {
                _ = merged_set.orderedRemove(i);
            } else {
                i += 1;
            }
        }

        const index = self.setLowerBound(merged_set.items);
        if (index >= self.sets.items.len or !std.mem.eql(u32, merged_set.items, self.sets.items[index])) {
            try self.sets.insert(self.allocator, index, try merged_set.toOwnedSlice());
            try self.unvisited_states.append(self.allocator, index);
            try self.set_to_dfa_state.insert(
                self.allocator,
                index,
                try dfa_builder.addState(.{}),
            );
        }

        std.debug.assert(index < self.set_to_dfa_state.items.len);
        return index;
    }

    fn setLowerBound(self: StateSetList, set: []const u32) SetIndex {
        std.debug.assert(std.sort.isSorted([]const u32, self.sets.items, {}, ascSliceU32));
        std.debug.assert(std.sort.isSorted(u32, set, {}, std.sort.asc(u32)));
        return @intCast(std.sort.lowerBound(
            []const u32,
            set,
            self.sets.items,
            {},
            ascSliceU32,
        ));
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

fn ascSliceU32(ctx: void, lhs: []const u32, rhs: []const u32) bool {
    _ = ctx;
    return std.mem.lessThan(u32, lhs, rhs);
}

pub fn addState(self: *Self, state: DFA.State) !u32 {
    try self.result.states.append(self.allocator, state);
    return @intCast(self.result.states.items.len - 1);
}

pub fn addStates(self: *Self, n: u32) !void {
    try self.result.states.appendNTimes(self.allocator, .{}, n);
}

pub fn markAtLineStart(self: *Self, i: u32) !void {
    std.debug.assert(i < self.result.states.items.len);
    const index = std.sort.lowerBound(
        u32,
        i,
        self.result.states_at_line_start,
        {},
        std.sort.asc(u32),
    );

    if (self.result.states_at_line_start[index] != i) {
        try self.result.states_at_line_start.insert(self.allocator, index, i);
    }
}

pub fn markAtLineEnd(self: *Self, i: u32) !void {
    std.debug.assert(i < self.result.states.items.len);
    const index = std.sort.lowerBound(
        u32,
        i,
        self.result.states_at_line_end,
        {},
        std.sort.asc(u32),
    );

    if (self.result.states_at_line_end[index] != i) {
        try self.result.states_at_line_end.insert(self.allocator, index, i);
    }
}

pub fn markStateAccepting(self: *Self, i: u32) !void {
    std.debug.assert(i < self.result.states.items.len);
    const index = std.sort.lowerBound(
        u32,
        i,
        self.result.accepting_states.items,
        {},
        std.sort.asc(u32),
    );

    const state_index_not_present = i >= self.result.accepting_states.items.len or self.result.accepting_states.items[index] != i;
    if (state_index_not_present) {
        try self.result.accepting_states.insert(self.allocator, index, i);
    }
}

pub fn addTransition(self: *Self, state: u32, transition: DFA.Transition) !void {
    const state_transitions_ranges = self.result.states.items[state].transitions.items(.range);
    const i = std.sort.lowerBound(
        DFA.Transition.Range,
        transition.range,
        state_transitions_ranges,
        {},
        DFA.Transition.Range.lessThan,
    );

    if (i >= state_transitions_ranges.len or !state_transitions_ranges[i].eq(transition.range)) {
        try self.result.states.items[state].transitions.insert(self.allocator, i, transition);
    }
}

fn resizeIfNeeded(bitset: *std.DynamicBitSetUnmanaged, allocator: std.mem.Allocator, n: u32) !void {
    if (n >= bitset.bit_length) {
        bitset.resize(allocator, n, false);
    }
}

test "basic" {
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

    var dfa_builder = DfaBuilder.init(allocator);
    var dfa = try dfa_builder.buildFromNFA(nfa);
    defer dfa.deinit(allocator);

    // dfa.debugPrint();
    try std.testing.expect(dfa.match("01110100")); // TODO
}
