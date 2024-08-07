const std = @import("std");
const NFA = @import("NFA.zig");

const Errors = @import("Errors.zig");

pub const RegexError = error{
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

fn handleError(pattern: []const u8, err: RegexError, errors: *Errors) !RegexCharacter {
    switch (err) {
        RegexError.TrailingBackslash => {
            try errors.addError("Pattern error: trailing backslash", .{});
            return RegexCharacter{ .Erroneous = 1 };
        },
        RegexError.UnknownEscapeSequence => {
            try errors.addError("Pattern error: unknown escape sequence `{s}`", .{pattern[0..2]});
            return RegexCharacter{ .Erroneous = 2 };
        },
    }
}

pub fn buildNFA(allocator: std.mem.Allocator, pattern: []const u8, errors: *Errors) !NFA {
    var automata = NFA.init(allocator);

    var i: usize = 0;
    var cur_state = try automata.addState(.{});
    while (getChar(pattern[i..]) catch |err| try handleError(pattern[i..], err, errors)) |c| : (i += 1) {
        switch (c) {
            .Special => {},
            .Literal => |l| {
                const new_state = try automata.addState(.{});

                try automata.addTransition(cur_state, NFA.Transition{
                    .symbol = l,
                    .dest_index = new_state,
                });
                cur_state = new_state;
            },
            .Erroneous => |to_skip| {
                i += to_skip - 1;
            },
        }
    }
    try automata.markStateAccepting(cur_state);

    return automata;
}

pub fn parseGroup() void {}

const RegexCharacter = union(enum) {
    Special: enum {
        LParen,
        RParen,
    },
    Literal: u8,
    Erroneous: u32, // chars to skip
};

fn getChar(input: []const u8) RegexError!?RegexCharacter {
    if (input.len == 0) {
        return null;
    }

    var cur_char = input[0];

    var escaped = false;
    if (cur_char == '\\') {
        escaped = true;
        if (input.len < 2) {
            return RegexError.TrailingBackslash;
        }
        cur_char = input[1];
    }

    return switch (cur_char) {
        '(' => RegexCharacter{ .Special = .LParen },
        ')' => RegexCharacter{ .Special = .RParen },
        else => |c| RegexCharacter{ .Literal = c },
    };
}
