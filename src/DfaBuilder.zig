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
        .result = .{},
    };
}

pub fn deinitResult(self: *Self) void {
    self.result.deinit(self.allocator);
}

pub fn build(self: *Self) DFA {
    defer self.result = undefined;
    return self.result;
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

pub fn addTransition(self: *Self, state: u32, key: u8, dest: u32) !void {
    try self.result.states.items[state].transitions.putNoClobber(self.allocator, key, dest);
}

fn resizeIfNeeded(bitset: *std.DynamicBitSetUnmanaged, allocator: std.mem.Allocator, n: u32) !void {
    if (n >= bitset.bit_length) {
        bitset.resize(allocator, n, false);
    }
}

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

    var dfa_builder = DfaBuilder.init(allocator);
    var dfa = try dfa_builder.buildFromNFA(nfa);
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

    var dfa_builder = DfaBuilder.init(allocator);
    var dfa = try dfa_builder.buildFromNFA(nfa);
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
