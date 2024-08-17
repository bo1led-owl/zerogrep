const std = @import("std");
const Self = @This();

allocator: std.mem.Allocator,
states: std.ArrayListUnmanaged(State) = .{},
accepting_state: u32 = 0,

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
}

pub fn debugPrint(nfa: Self) void {
    std.debug.print("accepting: {d}\n", .{nfa.accepting_state});
    for (0.., nfa.states.items) |i, state| {
        std.debug.print("State {d}\n", .{i});
        if (state.at_line_start) {
            std.debug.print("anchored ^\n", .{});
        }
        if (state.at_line_end) {
            std.debug.print("anchored $\n", .{});
        }

        for (0..state.transitions.len) |j| {
            const transition = state.transitions.get(j);

            if (transition.range.start == transition.range.end) {
                std.debug.print("\t{c} -> {d}\n", .{ transition.range.start, transition.dest_index });
            } else {
                std.debug.print("\t{d}-{d} -> {d}\n", .{ transition.range.start, transition.range.end, transition.dest_index });
            }
        }

        for (state.epsilon_transitions.items) |eps_transition| {
            std.debug.print("\teps -> {d}\n", .{eps_transition});
        }
    }
}

pub fn addState(self: *Self, state: State) !u32 {
    try self.states.append(self.allocator, state);
    return @intCast(self.states.items.len - 1);
}

pub fn markAtLineStart(self: *Self, i: u32) void {
    self.states.items[i].at_line_start = true;
}

pub fn markAtLineEnd(self: *Self, i: u32) void {
    self.states.items[i].at_line_end = true;
}

pub fn setAcceptingState(self: *Self, i: u32) void {
    std.debug.assert(i < self.states.items.len);
    self.accepting_state = i;
}

pub fn addTransition(self: *Self, state: u32, transition: Transition) !void {
    try self.states.items[state].addTransition(self.allocator, transition);
}

pub fn addEpsTransition(self: *Self, state: u32, dest_index: u32) !void {
    try self.states.items[state].addEpsTransition(self.allocator, dest_index);
}

pub const Transition = struct {
    pub const Range = struct {
        start: u8 = 0,
        end: u8 = 0,

        pub fn lessThan(ctx: void, lhs: Range, rhs: Range) bool {
            _ = ctx;
            if (lhs.start != rhs.start) {
                return lhs.start < rhs.start;
            }
            return lhs.end < rhs.end;
        }

        pub fn searchLessThan(ctx: void, lhs: Range, rhs: Range) bool {
            _ = ctx;
            return lhs.end < rhs.start;
        }

        pub fn matches(self: Range, c: u8) bool {
            return self.start <= c and c <= self.end;
        }
    };

    range: Range = .{},
    dest_index: u32 = 0,

    pub fn fromChar(c: u8, dest_index: u32) Transition {
        return .{
            .range = .{
                .start = c,
                .end = c,
            },
            .dest_index = dest_index,
        };
    }

    pub fn fromRange(start: u8, end: u8, dest_index: u32) Transition {
        return .{
            .range = .{
                .start = start,
                .end = end,
            },
            .dest_index = dest_index,
        };
    }
};

pub const State = struct {
    transitions: std.MultiArrayList(Transition) = .{},
    epsilon_transitions: std.ArrayListUnmanaged(u32) = .{},
    at_line_start: bool = false,
    at_line_end: bool = false,

    pub fn clone(self: State, allocator: std.mem.Allocator) !State {
        return State{
            .transitions = try self.transitions.clone(allocator),
            .epsilon_transitions = try self.epsilon_transitions.clone(allocator),
            .at_line_start = self.at_line_start,
            .at_line_end = self.at_line_end,
        };
    }

    pub fn cloneWithoutAnchors(self: State, allocator: std.mem.Allocator) !State {
        return State{
            .transitions = try self.transitions.clone(allocator),
            .epsilon_transitions = try self.epsilon_transitions.clone(allocator),
        };
    }

    pub fn deinit(self: *State, allocator: std.mem.Allocator) void {
        self.transitions.deinit(allocator);
        self.epsilon_transitions.deinit(allocator);
    }

    pub fn addTransition(self: *State, allocator: std.mem.Allocator, transition: Transition) !void {
        const i = std.sort.lowerBound(
            Transition.Range,
            transition.range,
            self.transitions.items(.range),
            {},
            Transition.Range.lessThan,
        );
        try self.transitions.insert(allocator, i, transition);
    }

    pub fn addEpsTransition(self: *State, allocator: std.mem.Allocator, dest_index: u32) !void {
        try self.epsilon_transitions.append(allocator, dest_index);
    }
};
