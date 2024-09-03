const std = @import("std");
const Self = @This();

states: std.ArrayListUnmanaged(State) = .{},
initial_state: u32 = 0,
states_at_line_start: std.ArrayListUnmanaged(u32) = .{},
states_at_line_end: std.ArrayListUnmanaged(u32) = .{},
accepting_states: std.ArrayListUnmanaged(u32) = .{},

pub const State = struct {
    transitions: std.MultiArrayList(Transition) = .{},

    pub fn deinit(self: *State, allocator: std.mem.Allocator) void {
        self.transitions.deinit(allocator);
    }
};

pub const Transition = struct {
    pub const Range = struct {
        start: u8 = 0,
        end: u8 = 0,

        pub fn eq(lhs: Range, rhs: Range) bool {
            return lhs.start == rhs.start and lhs.end == rhs.end;
        }

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
        for (0..state.transitions.len) |j| {
            const transition = state.transitions.get(j);

            if (transition.range.start == transition.range.end) {
                std.debug.print("\t{c} -> {d}\n", .{ transition.range.start, transition.dest_index });
            } else {
                std.debug.print("\t{d}-{d} -> {d}\n", .{ transition.range.start, transition.range.end, transition.dest_index });
            }
        }
    }
}

pub fn match(self: Self, line: []const u8) bool {
    for (0..line.len) |i| {
        if (self.walk(line[i..], i == 0)) {
            return true;
        }
    } else {
        return self.walk("", true);
    }

    return false;
}

fn walk(self: Self, input: []const u8, at_line_start: bool) bool {
    var cur_state = self.initial_state;
    for (0..input.len + 1) |i| {
        if (self.isStateAtLineStart(cur_state) and (!at_line_start or i != 0)) {
            return false;
        }

        if (self.isStateAtLineEnd(cur_state) and i < input.len) {
            return false;
        }

        if (self.isStateAccepting(cur_state)) {
            return true;
        }

        if (i >= input.len) {
            break;
        }

        const c = input[i];
        const dest_opt = self.getTransition(cur_state, c);
        if (dest_opt) |dest| {
            cur_state = dest;
        } else {
            return false;
        }
    }

    return false;
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
    const index = std.sort.lowerBound(
        Transition.Range,
        Transition.Range{ .start = key, .end = key },
        self.states.items[from].transitions.items(.range),
        {},
        Transition.Range.searchLessThan,
    );
    if (index >= self.states.items[from].transitions.len) {
        return null;
    }

    const transition = self.states.items[from].transitions.get(index);
    if (transition.range.matches(key)) {
        return transition.dest_index;
    } else {
        return null;
    }
}
