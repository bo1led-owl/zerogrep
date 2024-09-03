const std = @import("std");
const Self = @This();
const Builder = @import("NfaBuilder.zig");

states: std.ArrayListUnmanaged(State) = .{},
accepting_states: std.ArrayListUnmanaged(u32) = .{},

pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
    for (self.states.items) |*state| {
        state.deinit(allocator);
    }
    self.states.deinit(allocator);
    self.accepting_states.deinit(allocator);
}

pub fn debugPrint(nfa: Self) void {
    std.debug.print("accepting: {any}\n", .{nfa.accepting_states});
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

fn getFirstTransition(self: Self, from: u32, key: u8) u32 {
    return @intCast(std.sort.lowerBound(
        Transition.Range,
        Transition.Range{ .start = key, .end = key },
        self.states.items[from].transitions.items(.range),
        {},
        Transition.Range.searchLessThan,
    ));
}

pub const Stack = std.ArrayList(StackFrame);

const StackFrame = struct {
    state_index: u32,
    char_index: u32,
    cur_transition: u32,
    cur_epsilon_transition: u32,
};

fn walk(self: Self, stack: *Stack, input: []const u8, at_line_start: bool) !bool {
    try stack.append(.{
        .state_index = 0,
        .char_index = 0,
        .cur_transition = if (input.len > 0) self.getFirstTransition(0, input[0]) else 0,
        .cur_epsilon_transition = 0,
    });

    while (stack.items.len > 0) {
        const frame = stack.getLast();
        const state = &self.states.items.ptr[frame.state_index];

        if (state.*.at_line_start and (!at_line_start or frame.char_index != 0)) {
            _ = stack.pop();
            continue;
        }

        if (state.*.at_line_end and frame.char_index < input.len) {
            _ = stack.pop();
            continue;
        }

        if (std.mem.indexOfScalar(u32, self.accepting_states.items, frame.state_index) != null) {
            return true;
        }

        if (frame.cur_epsilon_transition < state.*.epsilon_transitions.items.len) {
            const dest = state.*.epsilon_transitions.items[frame.cur_epsilon_transition];

            // std.debug.print("{d} eps -> {d}\n", .{ frame.state_index, dest });

            stack.items[stack.items.len - 1].cur_epsilon_transition += 1;

            try stack.append(.{
                .state_index = dest,
                .char_index = frame.char_index,
                .cur_transition = if (frame.char_index + 1 < input.len)
                    self.getFirstTransition(dest, input[frame.char_index + 1])
                else
                    0,
                .cur_epsilon_transition = 0,
            });
            continue;
        }

        if (frame.char_index >= input.len) {
            _ = stack.pop();
            continue;
        }

        if (frame.cur_transition >= state.*.transitions.len) {
            _ = stack.pop();
            continue;
        }

        const transition = state.*.transitions.get(frame.cur_transition);
        const cur_char = input[frame.char_index];
        if (transition.range.matches(cur_char)) {
            // std.debug.print("{d} {c} -> {d}\n", .{ frame.state_index, cur_char, transition.dest_index });

            stack.items[stack.items.len - 1].cur_transition += 1;

            try stack.append(.{
                .state_index = transition.dest_index,
                .char_index = frame.char_index + 1,
                .cur_transition = if (frame.char_index + 1 < input.len)
                    self.getFirstTransition(transition.dest_index, input[frame.char_index + 1])
                else
                    0,
                .cur_epsilon_transition = 0,
            });
        } else {
            _ = stack.pop();
        }
    }

    return false;
}

pub fn match(self: Self, stack: *Stack, line: []const u8) !bool {
    defer stack.clearRetainingCapacity();

    // var timer = try std.time.Timer.start();

    // defer {
    //     const nfa_walk_time: f64 = @floatFromInt(timer.read());
    //     std.debug.print("NFA is walked in {d:.3}us\n", .{
    //         nfa_walk_time / std.time.ns_per_us,
    //     });
    // }

    for (0..line.len) |i| {
        if (try self.walk(stack, line[i..], i == 0)) {
            return true;
        }
        stack.clearRetainingCapacity();
    } else {
        return try self.walk(stack, "", true);
    }

    return false;
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

        pub fn eq(lhs: Range, rhs: Range) bool {
            return lhs.start == rhs.start and lhs.end == rhs.end;
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

    pub fn lessThan(ctx: void, lhs: Transition, rhs: Transition) bool {
        return Range.lessThan(ctx, lhs.range, rhs.range);
    }

    range: Range = .{},
    dest_index: u32 = 0,
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
};

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

    try builder.addTransition(0, Transition{ .range = Transition.Range.fromChar('a'), .dest_index = 1 });
    try builder.addTransition(1, Transition{ .range = Transition.Range.fromChar('b'), .dest_index = 2 });

    var nfa = builder.build();
    defer nfa.deinit(allocator);

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
    var builder = Builder.init(allocator);

    for (0..3) |_| {
        _ = try builder.addState(.{});
    }

    try builder.markStateAccepting(2);

    try builder.addTransition(0, Transition{ .range = Transition.Range.fromChar('a'), .dest_index = 1 });
    try builder.addEpsTransition(1, 2);

    var nfa = builder.build();
    defer nfa.deinit(allocator);

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
    var builder = Builder.init(allocator);

    for (0..5) |_| {
        _ = try builder.addState(.{});
    }
    try builder.markStateAccepting(4);

    try builder.addTransition(0, Transition{ .range = Transition.Range.fromChar('a'), .dest_index = 1 });
    try builder.addTransition(0, Transition{ .range = Transition.Range.fromChar('b'), .dest_index = 2 });
    try builder.addTransition(0, Transition{ .range = Transition.Range.fromChar('c'), .dest_index = 3 });

    try builder.addEpsTransition(1, 4);
    try builder.addEpsTransition(2, 4);
    try builder.addEpsTransition(3, 4);

    var nfa = builder.build();
    defer nfa.deinit(allocator);

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
    var builder = Builder.init(allocator);

    for (0..3) |_| {
        _ = try builder.addState(.{});
    }
    try builder.markStateAccepting(2);

    try builder.addTransition(0, Transition{ .range = Transition.Range.fromChar('a'), .dest_index = 1 });
    try builder.addEpsTransition(1, 0);
    try builder.addEpsTransition(1, 2);

    var nfa = builder.build();
    defer nfa.deinit(allocator);

    var stack = Stack.init(allocator);
    defer stack.deinit();

    try std.testing.expect(!try nfa.match(&stack, ""));
    try std.testing.expect(try nfa.match(&stack, "a"));
    try std.testing.expect(try nfa.match(&stack, "aa"));
    try std.testing.expect(try nfa.match(&stack, "baab"));
    try std.testing.expect(!try nfa.match(&stack, "b"));
    try std.testing.expect(!try nfa.match(&stack, "c"));
}
