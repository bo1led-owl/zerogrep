const std = @import("std");
const NFA = @import("NFA.zig");
const State = NFA.State;
const Transition = NFA.Transition;

const Self = @This();

allocator: std.mem.Allocator,
result: NFA,

pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .result = .{},
    };
}

pub fn build(self: *Self) NFA {
    defer self.result = undefined;
    return self.result;
}

pub fn addState(self: *Self, state: State) !u32 {
    try self.result.states.append(self.allocator, state);
    return @intCast(self.result.states.items.len - 1);
}

pub fn cloneRange(self: *Self, start: u32, end: u32) !u32 {
    std.debug.assert(start < self.result.states.items.len);
    std.debug.assert(end < self.result.states.items.len);
    std.debug.assert(start < end);
    for (start..end + 1) |i| {
        const new_state = try self.result.states.items[i].cloneWithoutAnchors(self.allocator);
        _ = try self.addState(new_state);
    }

    return end + 1;
}

pub fn markAtLineStart(self: *Self, i: u32) void {
    std.debug.assert(i < self.result.states.items.len);
    self.result.states.items[i].at_line_start = true;
}

pub fn markAtLineEnd(self: *Self, i: u32) void {
    std.debug.assert(i < self.result.states.items.len);
    self.result.states.items[i].at_line_end = true;
}

pub fn markStateAccepting(self: *Self, i: u32) !void {
    std.debug.assert(i < self.result.states.items.len);
    try self.result.accepting_states.append(self.allocator, i);
}

pub fn addTransition(self: *Self, state: u32, transition: Transition) !void {
    const i = std.sort.lowerBound(
        Transition.Range,
        transition.range,
        self.result.states.items[state].transitions.items(.range),
        {},
        Transition.Range.lessThan,
    );
    try self.result.states.items[state].transitions.insert(self.allocator, i, transition);
}

pub fn addEpsTransition(self: *Self, state: u32, dest_index: u32) !void {
    try self.result.states.items[state].epsilon_transitions.append(self.allocator, dest_index);
}
