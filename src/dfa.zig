const std = @import("std");

pub const DFA = struct {
    allocator: std.mem.Allocator,
    states: std.ArrayListUnmanaged(State),
    accepting_states: std.ArrayListUnmanaged(u32),

    pub fn init(allocator: std.mem.Allocator) DFA {
        return DFA{
            .allocator = allocator,
            .states = .{},
            .accepting_states = .{},
        };
    }
};

pub const Transition = struct {
    symbol: u8,
    dest_index: u32,
};

pub const State = struct {
    // TODO: keep transitions sorted, search with binsearch
    transitions: std.ArrayListUnmanaged(Transition),
};
