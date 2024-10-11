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

pub fn markInitial(self: *Self, i: u32) void {
    self.result.initial_state = i;
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

    const state_index_not_present = index >= self.result.accepting_states.items.len or self.result.accepting_states.items[index] != i;
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
