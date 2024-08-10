const std = @import("std");
const NFA = @import("NFA.zig");

const Errors = @import("Errors.zig");

const Self = @This();

gpa: std.mem.Allocator,
lexer: Lexer,
errors: *Errors,

pub fn init(gpa: std.mem.Allocator, errors: *Errors, pattern: []const u8) Self {
    return .{
        .gpa = gpa,
        .lexer = .{
            .input = pattern,
        },
        .errors = errors,
    };
}

const CharError = error{
    TrailingBackslash,
    UnknownEscapeSequence,
};

const Lexer = struct {
    input: []const u8,

    fn advance(self: *Lexer) ?u8 {
        if (self.input.len == 0) return null;

        defer self.input = self.input[1..];
        std.debug.print("{c}", .{self.input[0]});
        return self.input[0];
    }

    pub fn getChar(self: *Lexer) CharError!RegexCharacter {
        var cur_char = self.advance();
        if (cur_char == null) {
            return RegexCharacter.EOF;
        }

        var escaped = false;
        if (cur_char.? == '\\') {
            escaped = true;
            cur_char = self.advance();
            if (cur_char == null) {
                return CharError.TrailingBackslash;
            }
        }

        return switch (cur_char.?) {
            '(' => |c| if (escaped) RegexCharacter{ .Literal = c } else RegexCharacter.LParen,
            ')' => |c| if (escaped) RegexCharacter{ .Literal = c } else RegexCharacter.RParen,
            else => |c| RegexCharacter{ .Literal = c },
        };
    }
};

const RegexCharacter = union(enum) {
    EOF,
    LParen,
    RParen,
    Literal: u8,
    Erroneous,

    pub fn eq(lhs: RegexCharacter, rhs: RegexCharacter) bool {
        const tag = std.meta.Tag(RegexCharacter);
        if (@as(tag, lhs) != @as(tag, rhs)) {
            return false;
        }

        return switch (lhs) {
            .Literal => lhs.Literal == rhs.Literal,
            else => true,
        };
    }
};

pub fn isStringLiteral(pattern: []const u8) !bool {
    var lexer = Lexer{ .input = pattern };
    while (true) {
        const c = lexer.getChar() catch RegexCharacter.Erroneous;
        switch (c) {
            .EOF => break,
            .LParen, .RParen => return false,
            .Literal => {},
            .Erroneous => {
                return false;
            },
        }
    }
    return true;
}

fn handleError(self: *const Self, err: CharError) !RegexCharacter {
    switch (err) {
        CharError.TrailingBackslash => {
            try self.errors.addError("Pattern error: trailing backslash", .{});
            return RegexCharacter.Erroneous;
        },
        CharError.UnknownEscapeSequence => {
            try self.errors.addError("Pattern error: unknown escape sequence", .{});
            return RegexCharacter.Erroneous;
        },
    }
}

pub const AutomataBuildResult = struct {
    automata: NFA,
    errors_occured: bool,
};

pub fn buildNFA(self: *Self) !AutomataBuildResult {
    const initial_error_count = self.errors.count();

    var nfa = NFA.init(self.gpa);

    const cur_state = try nfa.addState(.{});
    const res = try self.parse(&nfa, cur_state, RegexCharacter.EOF);

    // std.debug.print("`{s}`\n", .{self.lexer.input});
    // std.debug.assert(res.characters_parsed == pattern.len);

    nfa.setAcceptingState(res.accepting_state);

    return .{
        .automata = nfa,
        .errors_occured = self.errors.count() != initial_error_count,
    };
}

const ParseResult = struct {
    accepting_state: u32,
};

fn parse(self: *Self, nfa: *NFA, initial_state: u32, parse_until: RegexCharacter) !ParseResult {
    std.debug.assert(switch (parse_until) {
        .EOF => true,
        .RParen => true,
        else => false,
    });

    var cur_state = initial_state;
    var finished_correctly = false;
    while (true) {
        const regex_char = self.lexer.getChar() catch |err| try self.handleError(err);
        if (regex_char.eq(parse_until)) {
            finished_correctly = true;
            break;
        }

        switch (regex_char) {
            .EOF => break,
            .LParen => {
                const result = try self.parse(nfa, cur_state, RegexCharacter.RParen);
                cur_state = result.accepting_state;
            },
            .RParen => {
                try self.errors.addError("Regex error: unmatched `)`", .{});
            },
            .Literal => |literal_char| {
                const new_state = try nfa.addState(.{});

                try nfa.addTransition(cur_state, NFA.Transition{
                    .symbol = literal_char,
                    .dest_index = new_state,
                });
                cur_state = new_state;
            },
            .Erroneous => {},
        }
    }

    if (!finished_correctly) {
        switch (parse_until) {
            .RParen => try self.errors.addError("Regex error: unmatched `(`", .{}),
            else => unreachable,
        }
    }

    return .{
        .accepting_state = cur_state,
    };
}
