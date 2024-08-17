const std = @import("std");
const Self = @This();

allocator: std.mem.Allocator,
states: std.ArrayListUnmanaged(State) = .{},
states_at_line_start: std.DynamicBitSetUnmanaged = .{},
states_at_line_end: std.DynamicBitSetUnmanaged = .{},
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
    self.states_at_line_start.deinit(self.allocator);
    self.states_at_line_end.deinit(self.allocator);
}

// TODO: debugPrint

fn getFirstTransition(self: Self, from: u32, key: u8) u32 {
    return @intCast(std.sort.lowerBound(
        Transition.Range,
        Transition.Range{ .start = key, .end = key },
        self.states.items[from].transitions.items(.range),
        {},
        Transition.Range.searchLessThan,
    ));
}

pub fn addState(self: *Self, state: State) !u32 {
    try self.states.append(self.allocator, state);
    return @intCast(self.states.items.len - 1);
}

pub fn markAtLineStart(self: *Self, i: u32) void {
    std.debug.assert(i < self.states.items.len);
    self.states.items[i].at_line_start = true;
}

pub fn markAtLineEnd(self: *Self, i: u32) void {
    std.debug.assert(i < self.states.items.len);
    self.states.items[i].at_line_end = true;
}

pub fn setAcceptingState(self: *Self, i: u32) void {
    std.debug.assert(i < self.states.items.len);
    self.accepting_state = i;
}

pub fn addTransition(self: *Self, state: u32, transition: Transition) !void {
    try self.states.items[state].addTransition(self.allocator, transition);
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

        pub fn fromChar(c: u8) Range {
            return .{
                .start = c,
                .end = c,
            };
        }

        pub fn fromRange(start: u8, end: u8) Range {
            return .{
                .start = start,
                .end = end,
            };
        }
    };

    range: Range = .{},
    dest_index: u32 = 0,
};

pub const State = struct {
    transitions: std.MultiArrayList(Transition) = .{},

    pub fn deinit(self: *State, allocator: std.mem.Allocator) void {
        self.transitions.deinit(allocator);
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
};
