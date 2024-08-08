const std = @import("std");
const NFA = @import("NFA.zig");

const Errors = @import("Errors.zig");

const Self = @This();

gpa: std.mem.Allocator,
errors: *Errors,

pub fn init(gpa: std.mem.Allocator, errors: *Errors) Self {
    return .{
        .gpa = gpa,
        .errors = errors,
    };
}

pub const Error = error{
    TrailingBackslash,
    UnknownEscapeSequence,
};

pub fn isStringLiteral(pattern: []const u8) !bool {
    var i: usize = 0;
    while (getChar(pattern[i..]) catch RegexCharacter{ .Erroneous = 0 }) |c| : (i += 1) {
        switch (c) {
            .Special => {
                return false;
            },
            .Literal => {},
            .Erroneous => {
                return false;
            },
        }
    }
    return true;
}

fn handleError(self: *const Self, pattern: []const u8, err: Error) !RegexCharacter {
    switch (err) {
        Error.TrailingBackslash => {
            try self.errors.addError("Pattern error: trailing backslash", .{});
            return RegexCharacter{ .Erroneous = 1 };
        },
        Error.UnknownEscapeSequence => {
            try self.errors.addError("Pattern error: unknown escape sequence `{s}`", .{pattern[0..2]});
            return RegexCharacter{ .Erroneous = 2 };
        },
    }
}

pub const AutomataBuildResult = struct {
    automata: NFA,
    errors_occured: bool,
};

pub fn buildNFA(self: *const Self, pattern: []const u8) !AutomataBuildResult {
    const initial_error_count = self.errors.count();

    var nfa = NFA.init(self.gpa);

    var i: u32 = 0;
    var cur_state = try nfa.addState(.{});
    while (getChar(pattern[i..]) catch |err|
        try self.handleError(pattern[i..], err)) |regex_char| : (i += 1)
    {
        switch (regex_char) {
            .Special => |special_char| {
                switch (special_char) {
                    .LParen => {
                        const group_desc = try self.parseGroup(&nfa, cur_state, pattern[i + 1..]);
                        cur_state = group_desc.accepting_state;
                        i += group_desc.characters_parsed;
                    },
                    .RParen => {},
                }
            },
            .Literal => |literal_char| {
                const new_state = try nfa.addState(.{});

                try nfa.addTransition(cur_state, NFA.Transition{
                    .symbol = literal_char,
                    .dest_index = new_state,
                });
                cur_state = new_state;
            },
            .Erroneous => |to_skip| {
                i += to_skip - 1;
            },
        }
    }
    nfa.setAcceptingState(cur_state);

    return .{
        .automata = nfa,
        .errors_occured = self.errors.count() != initial_error_count,
    };
}

const GroupDesc = struct {
    accepting_state: u32,
    characters_parsed: u32,
};

fn parseGroup(self: *const Self, nfa: *NFA, initial_state: u32, pattern: []const u8) !GroupDesc {
    var i: u32 = 0;
    var cur_state = initial_state;
    while (getChar(pattern[i..]) catch |err|
        try self.handleError(pattern[i..], err)) |regex_char| : (i += 1)
    {
        switch (regex_char) {
            .Special => |special_char| {
                switch (special_char) {
                    .LParen => {
                        const group_desc = try self.parseGroup(nfa, cur_state, pattern[i + 1..]);
                        cur_state = group_desc.accepting_state;
                        i += group_desc.characters_parsed;
                    },
                    .RParen => {
                        break;
                    },
                }
            },
            .Literal => |literal_char| {
                const new_state = try nfa.addState(.{});

                try nfa.addTransition(cur_state, NFA.Transition{
                    .symbol = literal_char,
                    .dest_index = new_state,
                });
                cur_state = new_state;
            },
            .Erroneous => |to_skip| {
                i += to_skip - 1;
            },
        }
    }

    return .{
        .accepting_state = cur_state,
        .characters_parsed = i,
    };
}

const RegexCharacter = union(enum) {
    Special: enum {
        LParen,
        RParen,
    },
    Literal: u8,
    Erroneous: u32, // chars to skip
};

fn getChar(input: []const u8) Error!?RegexCharacter {
    if (input.len == 0) {
        return null;
    }

    var cur_char = input[0];

    var escaped = false;
    if (cur_char == '\\') {
        escaped = true;
        if (input.len < 2) {
            return Error.TrailingBackslash;
        }
        cur_char = input[1];
    }

    return switch (cur_char) {
        '(' => RegexCharacter{ .Special = .LParen },
        ')' => RegexCharacter{ .Special = .RParen },
        else => |c| RegexCharacter{ .Literal = c },
    };
}
