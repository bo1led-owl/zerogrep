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

fn getTransitions(self: *const Self, from: u32, key: u8) TransitionIterator {
    const state = self.states.items[from];
    const range = std.sort.equalRange(
        Transition.Range,
        Transition.Range{ .start = key, .end = key },
        state.transitions.items(.range),
        {},
        Transition.Range.searchLessThan,
    );

    return TransitionIterator{
        .transitions = state.transitions.items(.dest_index)[range[0]..range[1]],
    };
}

fn getEpsTransitions(self: *const Self, from: u32) TransitionIterator {
    const state = self.states.items[from];
    return TransitionIterator{ .transitions = state.epsilon_transitions.items };
}

pub const Stack = std.ArrayList(StackFrame);

const StackFrame = struct {
    state_index: u32,
    input_start: u32,
    iter: TransitionIterator,
    epsilon_iter: TransitionIterator,
};

fn walk(self: *const Self, stack: *Stack, input: []const u8, at_line_start: bool) !bool {
    try stack.append(.{
        .state_index = 0,
        .input_start = 0,
        .iter = if (input.len > 0) self.getTransitions(0, input[0]) else .{},
        .epsilon_iter = self.getEpsTransitions(0),
    });

    while (stack.items.len > 0) {
        const state_index = stack.getLast().state_index;
        const char_index = stack.getLast().input_start;
        var iter = &stack.*.items[stack.items.len - 1].iter;
        var epsilon_iter = &stack.*.items[stack.items.len - 1].epsilon_iter;

        if (self.states.items[state_index].at_line_start and (!at_line_start or char_index != 0)) {
            _ = stack.popOrNull();
            continue;
        }

        if (self.states.items[state_index].at_line_end and char_index < input.len) {
            _ = stack.popOrNull();
            continue;
        }

        if (self.accepting_state == state_index) {
            return true;
        }

        if (epsilon_iter.next()) |dest| {
            // std.debug.print("{d} eps -> {d}\n", .{ state_index, dest });

            const new_iter = if (char_index < input.len) self.getTransitions(dest, input[char_index]) else TransitionIterator{};
            const new_eps_iter = self.getEpsTransitions(dest);

            try stack.append(.{
                .state_index = dest,
                .input_start = char_index,
                .iter = new_iter,
                .epsilon_iter = new_eps_iter,
            });
            continue;
        }

        if (char_index >= input.len) {
            _ = stack.popOrNull();
            continue;
        }

        if (iter.next()) |dest| {
            // std.debug.print("{d} -> {d}\n", .{ state_index, dest });

            const new_iter = if (char_index + 1 < input.len) self.getTransitions(dest, input[char_index + 1]) else TransitionIterator{};
            const new_eps_iter = self.getEpsTransitions(dest);

            try stack.append(.{
                .state_index = dest,
                .input_start = char_index + 1,
                .iter = new_iter,
                .epsilon_iter = new_eps_iter,
            });
        } else {
            _ = stack.popOrNull();
        }
    }

    return false;
}

pub fn match(self: *const Self, stack: *Stack, line: []const u8) !bool {
    // std.debug.print("accepting: {d}\n", .{self.accepting_state});
    defer stack.clearRetainingCapacity();

    // var timer = try std.time.Timer.start();

    // defer {
    //     const nfa_walk_time: f64 = @floatFromInt(timer.read());
    //     std.debug.print("NFA is walked in {d:.3}us\n", .{
    //         nfa_walk_time / std.time.ns_per_us,
    //     });
    // }

    for (0..line.len) |i| {
        // std.debug.print("{s}\n", .{line[i..]});

        if (try self.walk(stack, line[i..], i == 0)) {
            return true;
        }
        stack.clearRetainingCapacity();
    } else {
        return try self.walk(stack, "", true);
    }

    return false;
}

/// add state and return its index
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

test "basic" {
    //      a        b
    // (0) ---> (1) ---> ((2))
    //

    const allocator = std.testing.allocator;
    var nfa = Self.init(allocator);
    defer nfa.deinit();

    for (0..3) |_| {
        _ = try nfa.addState(.{});
    }

    nfa.setAcceptingState(2);

    try nfa.addTransition(0, Transition.fromChar('a', 1));
    try nfa.addTransition(1, Transition.fromChar('b', 2));

    var stack = Stack.init(allocator);
    defer stack.deinit();
    try std.testing.expect(try nfa.match(&stack, "ab"));
    try std.testing.expect(try nfa.match(&stack, "cab"));
    try std.testing.expect(try nfa.match(&stack, "abc"));
    try std.testing.expect(!try nfa.match(&stack, "a"));
    try std.testing.expect(!try nfa.match(&stack, "b"));
    try std.testing.expect(!try nfa.match(&stack, ""));
    try std.testing.expect(!try nfa.match(&stack, "ac"));
}

test "epsilon transition" {
    //      a        eps
    // (0) ---> (1) ----> ((2))
    //
    const allocator = std.testing.allocator;
    var nfa = Self.init(allocator);
    defer nfa.deinit();

    for (0..3) |_| {
        _ = try nfa.addState(.{});
    }

    nfa.setAcceptingState(2);

    try nfa.addTransition(0, Transition.fromChar('a', 1));
    try nfa.addEpsTransition(1, 2);

    var stack = Stack.init(allocator);
    defer stack.deinit();
    try std.testing.expect(try nfa.match(&stack, "ab"));
    try std.testing.expect(try nfa.match(&stack, "cab"));
    try std.testing.expect(try nfa.match(&stack, "abc"));
    try std.testing.expect(try nfa.match(&stack, "a"));
    try std.testing.expect(!try nfa.match(&stack, "b"));
    try std.testing.expect(!try nfa.match(&stack, ""));
}

test "branching" {
    //      a
    //    ----> (1) --\
    //  /              \ eps
    //  |   b       eps \
    // (0) ---> (2) -----> ((4))
    //  |               /
    //  \   c          / eps
    //   -----> (3) --/

    const allocator = std.testing.allocator;
    var nfa = Self.init(allocator);
    defer nfa.deinit();

    for (0..5) |_| {
        _ = try nfa.addState(.{});
    }
    nfa.setAcceptingState(4);

    try nfa.addTransition(0, Transition.fromChar('a', 1));
    try nfa.addTransition(0, Transition.fromChar('b', 2));
    try nfa.addTransition(0, Transition.fromChar('c', 3));

    try nfa.addEpsTransition(1, 4);
    try nfa.addEpsTransition(2, 4);
    try nfa.addEpsTransition(3, 4);

    var stack = Stack.init(allocator);
    defer stack.deinit();

    try std.testing.expect(try nfa.match(&stack, "ab"));
    try std.testing.expect(try nfa.match(&stack, "cab"));
    try std.testing.expect(try nfa.match(&stack, "abc"));
    try std.testing.expect(try nfa.match(&stack, "a"));
    try std.testing.expect(try nfa.match(&stack, "b"));
    try std.testing.expect(try nfa.match(&stack, "c"));
    try std.testing.expect(!try nfa.match(&stack, ""));
    try std.testing.expect(!try nfa.match(&stack, "d"));
    try std.testing.expect(!try nfa.match(&stack, "foo"));
}

test "loop" {
    //      a        eps
    // (0) ---> (1) ---> ((2))
    //  ^ -------
    //      eps

    const allocator = std.testing.allocator;
    var nfa = Self.init(allocator);
    defer nfa.deinit();

    for (0..3) |_| {
        _ = try nfa.addState(.{});
    }
    nfa.setAcceptingState(2);

    try nfa.addTransition(0, Transition.fromChar('a', 1));
    try nfa.addEpsTransition(1, 0);
    try nfa.addEpsTransition(1, 2);

    var stack = Stack.init(allocator);
    defer stack.deinit();

    try std.testing.expect(!try nfa.match(&stack, ""));
    try std.testing.expect(try nfa.match(&stack, "a"));
    try std.testing.expect(try nfa.match(&stack, "aa"));
    try std.testing.expect(try nfa.match(&stack, "baab"));
    try std.testing.expect(!try nfa.match(&stack, "b"));
    try std.testing.expect(!try nfa.match(&stack, "c"));
}
