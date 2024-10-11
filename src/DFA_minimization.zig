const std = @import("std");
const DFA = @import("DFA.zig");
const DfaBuilder = @import("DfaBuilder.zig");

/// https://en.wikipedia.org/wiki/DFA_minimization
pub fn minimizeDFA(gpa: std.mem.Allocator, arena: std.mem.Allocator, dfa: DFA) !DFA {
    var builder = DfaBuilder.init(gpa);
    errdefer builder.deinitResult();

    var inverse_transitions = try InserseTransitions.init(gpa, dfa);
    defer inverse_transitions.deinit(gpa);

    var queue = Queue(StatePair).init(gpa);
    defer queue.deinit();

    const null_state_index: u32 = @intCast(dfa.states.items.len);
    var is_distinguishable = try StateTable.init(arena, dfa.states.items.len + 1);
    for (0..dfa.states.items.len + 1) |i| {
        for (0..dfa.states.items.len + 1) |j| {
            const i_u32: u32 = @intCast(i);
            const j_u32: u32 = @intCast(j);

            if (i != j and (i_u32 == null_state_index or j_u32 == null_state_index or dfa.isStateAccepting(i_u32) != dfa.isStateAccepting(j_u32))) {
                is_distinguishable.set(i_u32, j_u32);
                try queue.push(.{ .a = i_u32, .b = j_u32 });
            }
        }
    }

    while (!queue.isEmpty()) {
        const pair = try queue.popFront();

        // std.debug.print("pair <{d}, {d}>\n", .{pair.a, pair.b});

        var iter = inverse_transitions.repr[pair.a].iterator();
        while (iter.next()) |entry| {
            const a_ancestors = entry.value_ptr.items;
            const b_ancestors = inverse_transitions.getAncestors(pair.b, entry.key_ptr.*);

            for (a_ancestors) |r| {
                for (b_ancestors) |s| {
                    // std.debug.print("{d} {d}\n", .{ r, s });
                    if (!is_distinguishable.get(r, s)) {
                        is_distinguishable.set(r, s);
                        try queue.push(.{ .a = r, .b = s });
                    }
                }
            }
        }
    }

    // debug
    // dfa.debugPrint();

    // std.debug.print("\n  n ", .{});
    // for (0..dfa.states.items.len) |i| {
    //     std.debug.print("{d} ", .{i});
    // }
    // std.debug.print("\nn ", .{});
    // std.debug.print("{d} ", .{@intFromBool(is_distinguishable.get(null_state_index, null_state_index))});
    // for (0..dfa.states.items.len) |i| {
    //     std.debug.print(
    //         "{d} ",
    //         .{@intFromBool(is_distinguishable.get(
    //             null_state_index,
    //             @intCast(i),
    //         ))},
    //     );
    // }
    // std.debug.print("\n", .{});
    // for (0..dfa.states.items.len) |i| {
    //     std.debug.print("{d} ", .{i});
    //     std.debug.print("{d} ", .{@intFromBool(is_distinguishable.get(@intCast(i), null_state_index))});
    //     for (0..dfa.states.items.len) |j| {
    //         std.debug.print(
    //             "{d} ",
    //             .{@intFromBool(is_distinguishable.get(@intCast(i), @intCast(j)))},
    //         );
    //     }
    //     std.debug.print("\n", .{});
    // }
    // std.debug.print("\n", .{});
    // debug end

    const state_to_component: []?u32 = try gpa.alloc(?u32, dfa.states.items.len);
    defer gpa.free(state_to_component);
    @memset(state_to_component, null);

    var components_count: u32 = 0;
    for (0..dfa.states.items.len) |dfa_state| {
        if (state_to_component[dfa_state] == null) {
            components_count += 1;
            state_to_component[dfa_state] = components_count - 1;
        }

        for (dfa_state + 1..dfa.states.items.len) |another_dfa_state| {
            if (!is_distinguishable.get(@intCast(dfa_state), @intCast(another_dfa_state))) {
                state_to_component[another_dfa_state] = components_count - 1;
            }
        }
    }

    // for (0.., state_to_component) |i, c| {
    //     std.debug.print("{d} in {d}\n", .{ i, c.? });
    // }

    try builder.addStates(components_count);
    for (0.., dfa.states.items) |i, state| {
        // std.debug.print("state {d}\n", .{i});

        if (dfa.isStateAccepting(@intCast(i))) {
            try builder.markStateAccepting(state_to_component[i].?);
        }

        const cur_component = state_to_component[i].?;
        var iter = state.transitions.iterator();

        while (iter.next()) |entry| {
            // std.debug.print("from {d} by {c} -> {d} (comp {d})\n", .{ cur_component, entry.key_ptr.*, entry.value_ptr.*, state_to_component[entry.value_ptr.*].? });

            if (!builder.result.states.items[cur_component].transitions.contains(entry.key_ptr.*)) {
                try builder.addTransition(cur_component, entry.key_ptr.*, state_to_component[entry.value_ptr.*].?);
            } else {
                const existing_dest_component = builder.result.states.items[cur_component].transitions.get(entry.key_ptr.*).?;
                const new_dest_component = state_to_component[entry.value_ptr.*].?;
                // std.debug.print("{d} {d}\n", .{ existing_dest_component, new_dest_component });
                std.debug.assert(existing_dest_component == new_dest_component);
            }
        }
    }

    builder.markInitial(state_to_component[dfa.initial_state].?);

    return builder.build();
}

const InserseTransitions = struct {
    const Self = @This();

    pub const AncestorsMap = std.AutoArrayHashMapUnmanaged(u8, std.ArrayListUnmanaged(u32));

    /// a slice where each `i` is corresponding with a map from symbol
    /// to a set of states which have a transition to `i` by that symbol
    repr: []AncestorsMap,

    pub fn init(allocator: std.mem.Allocator, dfa: DFA) !Self {
        var self = Self{ .repr = try allocator.alloc(AncestorsMap, dfa.states.items.len + 1) };
        const null_state_index: u32 = @intCast(self.repr.len - 1);

        for (self.repr) |*map| {
            map.* = .{};
        }

        for (0.., dfa.states.items) |state_index, state| {
            for (std.math.minInt(u8)..std.math.maxInt(u8) + 1) |c| {
                const key: u8 = @intCast(c);

                const transition_result = state.transitions.get(key);
                if (transition_result) |transition_dest| {
                    const result = try self.repr[transition_dest].getOrPut(allocator, key);
                    if (!result.found_existing) {
                        result.value_ptr.* = std.ArrayListUnmanaged(u32){};
                    }

                    var set_ptr = result.value_ptr;

                    const index = std.sort.lowerBound(u32, @as(u32, @intCast(state_index)), set_ptr.items, {}, std.sort.asc(u32));
                    if (index >= set_ptr.items.len or set_ptr.items[index] != state_index) {
                        try set_ptr.insert(allocator, index, @intCast(state_index));
                    }

                    std.debug.assert(std.sort.isSorted(u32, set_ptr.items, {}, std.sort.asc(u32)));
                } else {
                    const result = try self.repr[null_state_index].getOrPut(allocator, key);
                    if (!result.found_existing) {
                        result.value_ptr.* = std.ArrayListUnmanaged(u32){};
                    }

                    var set_ptr = result.value_ptr;

                    const index = std.sort.lowerBound(u32, @as(u32, @intCast(state_index)), set_ptr.items, {}, std.sort.asc(u32));
                    if (index >= set_ptr.items.len or set_ptr.items[index] != state_index) {
                        try set_ptr.insert(allocator, index, @intCast(state_index));
                    }

                    std.debug.assert(std.sort.isSorted(u32, set_ptr.items, {}, std.sort.asc(u32)));
                }
            }
        }

        return self;
    }

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        for (self.repr) |*map| {
            var iter = map.iterator();
            while (iter.next()) |entry| {
                entry.value_ptr.*.deinit(allocator);
            }
            map.deinit(allocator);
        }
        allocator.free(self.repr);
    }

    pub fn getAncestors(self: Self, state: u32, key: u8) []const u32 {
        if (self.repr[state].get(key)) |result| {
            return result.items;
        }
        return &[0]u32{};
    }
};

fn ascSliceU32(ctx: void, lhs: []const u32, rhs: []const u32) bool {
    _ = ctx;
    return std.mem.lessThan(u32, lhs, rhs);
}

const StatePair = struct { a: u32, b: u32 };

fn Queue(comptime T: type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        left: std.ArrayListUnmanaged(T),
        right: std.ArrayListUnmanaged(T),

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .allocator = allocator,
                .left = .{},
                .right = .{},
            };
        }

        pub fn deinit(self: *Self) void {
            self.left.deinit(self.allocator);
            self.right.deinit(self.allocator);
        }

        pub fn push(self: *Self, x: T) !void {
            try self.left.append(self.allocator, x);
        }

        pub fn popFront(self: *Self) !T {
            if (self.right.items.len != 0) {
                return self.right.pop();
            } else {
                while (self.left.items.len != 0) {
                    try self.right.append(self.allocator, self.left.pop());
                }
                return self.right.pop();
            }
        }

        pub fn isEmpty(self: Self) bool {
            return self.left.items.len == 0 and self.right.items.len == 0;
        }
    };
}

const StateTable = struct {
    const Self = @This();

    repr: []std.DynamicBitSetUnmanaged,

    pub fn init(arena: std.mem.Allocator, state_count: usize) std.mem.Allocator.Error!Self {
        const repr = try arena.alloc(std.DynamicBitSetUnmanaged, state_count);

        for (0..state_count) |i| {
            repr[i] = try std.DynamicBitSetUnmanaged.initEmpty(arena, state_count);
        }
        return Self{
            .repr = repr,
        };
    }

    pub fn set(self: *Self, a: u32, b: u32) void {
        std.debug.assert(a != b);

        self.repr[a].set(b);
        self.repr[b].set(a);
    }

    pub fn get(self: *Self, a: u32, b: u32) bool {
        return self.repr[a].isSet(b);
    }
};

test "basic" {
    const allocator = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var builder = DfaBuilder.init(allocator);
    try builder.addStates(7);
    builder.markInitial(0);

    try builder.addTransition(0, 'f', 1);
    try builder.addTransition(1, 'o', 2);
    try builder.addTransition(2, 'o', 6);

    try builder.addTransition(0, 'b', 4);
    try builder.addTransition(4, 'a', 5);
    try builder.addTransition(5, 'r', 6);

    try builder.markStateAccepting(6);

    var dfa = builder.build();
    defer dfa.deinit(allocator);

    var min_dfa = try minimizeDFA(allocator, arena.allocator(), dfa);
    defer min_dfa.deinit(allocator);

    // min_dfa.debugPrint();

    try std.testing.expect(!(min_dfa.match(true, "") != null));
    try std.testing.expect((min_dfa.match(true, "foo") != null));
    try std.testing.expect((min_dfa.match(true, "bar") != null));
    try std.testing.expect(!(min_dfa.match(true, "b") != null));
    try std.testing.expect(!(min_dfa.match(true, "f") != null));
    try std.testing.expect(!(min_dfa.match(true, "far") != null));
    try std.testing.expect(!(min_dfa.match(true, "boo") != null));
}
