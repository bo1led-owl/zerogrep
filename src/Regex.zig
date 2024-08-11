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

        if (!escaped) {
            return switch (cur_char.?) {
                '*' => RegexCharacter.Asterisk,
                '+' => RegexCharacter.Plus,
                '?' => RegexCharacter.QuestionMark,
                '(' => RegexCharacter.LParen,
                ')' => RegexCharacter.RParen,
                '|' => RegexCharacter.Pipe,
                '^' => RegexCharacter.Caret,
                '$' => RegexCharacter.Dollar,
                else => |c| RegexCharacter{ .Literal = c },
            };
        } else {
            return switch (cur_char.?) {
                '(', ')', '|', '*', '+', '?', '^', '$' => |c| RegexCharacter{ .Literal = c },
                else => CharError.UnknownEscapeSequence,
            };
        }
    }
};

const RegexCharacter = union(enum) {
    EOF,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Pipe,
    QuestionMark,
    Asterisk,
    Plus,
    Caret,
    Dollar,
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
            .Literal => {},
            else => {
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

    var branches = std.ArrayListUnmanaged(u32){}; // indices of branches ends

    var cur_state = initial_state;
    var prev_state: u32 = undefined;
    var finished_correctly = false;
    while (true) {
        const regex_char = self.lexer.getChar() catch |err| try self.handleError(err);
        if (regex_char.eq(parse_until)) {
            finished_correctly = true;
            if (branches.items.len > 0) {
                try branches.append(self.gpa, cur_state);
            }
            break;
        }

        switch (regex_char) {
            .EOF => break,
            .Caret => {
                nfa.markAtLineStart(cur_state);
            },
            .Dollar => {
                nfa.markAtLineEnd(cur_state);
            },
            .LBracket => {
                // _ = self.parseBracketExpr();
            },
            .RBracket => {
                try self.errors.addError("Regex error: unmatched `]`", .{});
            },
            .LParen => {
                const result = try self.parse(nfa, cur_state, RegexCharacter.RParen);
                prev_state = cur_state;
                cur_state = result.accepting_state;
            },
            .RParen => {
                try self.errors.addError("Regex error: unmatched `)`", .{});
            },
            .Pipe => {
                try branches.append(self.gpa, cur_state);
                prev_state = cur_state;
                cur_state = initial_state;
            },
            .Asterisk => {
                const new_state = try nfa.addState(.{});

                try nfa.addEpsTransition(cur_state, prev_state);
                try nfa.addEpsTransition(cur_state, new_state);
                try nfa.addEpsTransition(prev_state, new_state);
                prev_state = cur_state;
                cur_state = new_state;
            },
            .QuestionMark => {
                try nfa.addEpsTransition(prev_state, cur_state);
                prev_state = cur_state;
            },
            .Plus => {
                var last_state: u32 = undefined;
                for (prev_state..cur_state + 1) |state| {
                    last_state = try nfa.addState(try nfa.states.items[state].clone(self.gpa));
                }
                try nfa.addEpsTransition(cur_state, last_state);
                try nfa.addEpsTransition(cur_state, cur_state + 1);
                prev_state = cur_state;
                cur_state = last_state;
            },
            .Literal => |literal_char| {
                const new_state = try nfa.addState(.{});

                try nfa.addTransition(cur_state, NFA.Transition.fromChar(literal_char, new_state));
                prev_state = cur_state;
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

    if (branches.items.len > 0) {
        cur_state = try nfa.addState(.{});

        for (branches.items) |state| {
            try nfa.addEpsTransition(state, cur_state);
        }
        branches.deinit(self.gpa);
    }

    return .{
        .accepting_state = cur_state,
    };
}

test "basic" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var errors = Errors.init(allocator, arena);
    defer errors.deinit();

    var regex = Self.init(allocator, &errors, "foobar");
    const nfa_result = try regex.buildNFA();
    try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
    try std.testing.expect(!nfa_result.errors_occured);

    var nfa = nfa_result.automata;
    defer nfa.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    try std.testing.expect(try nfa.match(&nfa_stack, "foobar"));
    try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
    try std.testing.expect(try nfa.match(&nfa_stack, "bazfoobar"));
    try std.testing.expect(!try nfa.match(&nfa_stack, ""));
    try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    try std.testing.expect(!try nfa.match(&nfa_stack, "bar"));
    try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
}

test "group basic" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var errors = Errors.init(allocator, arena);
    defer errors.deinit();

    var regex = Self.init(allocator, &errors, "(fo(oba)r)");
    const nfa_result = try regex.buildNFA();
    try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
    try std.testing.expect(!nfa_result.errors_occured);

    var nfa = nfa_result.automata;
    defer nfa.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    try std.testing.expect(try nfa.match(&nfa_stack, "foobar"));
    try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
    try std.testing.expect(try nfa.match(&nfa_stack, "bazfoobar"));
    try std.testing.expect(!try nfa.match(&nfa_stack, ""));
    try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    try std.testing.expect(!try nfa.match(&nfa_stack, "bar"));
    try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
}

test "alternatives" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var errors = Errors.init(allocator, arena);
    defer errors.deinit();

    var regex = Self.init(allocator, &errors, "foo|bar|baz");
    const nfa_result = try regex.buildNFA();
    try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
    try std.testing.expect(!nfa_result.errors_occured);

    var nfa = nfa_result.automata;
    defer nfa.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    try std.testing.expect(try nfa.match(&nfa_stack, "foobar"));
    try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
    try std.testing.expect(try nfa.match(&nfa_stack, "bazfoobar"));
    try std.testing.expect(try nfa.match(&nfa_stack, "foo"));
    try std.testing.expect(try nfa.match(&nfa_stack, "bar"));
    try std.testing.expect(try nfa.match(&nfa_stack, "baz"));
    try std.testing.expect(!try nfa.match(&nfa_stack, ""));
    try std.testing.expect(!try nfa.match(&nfa_stack, "random"));
    try std.testing.expect(!try nfa.match(&nfa_stack, "word"));
}

test "repeating single char" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "foo*");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "foobar"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
        try std.testing.expect(try nfa.match(&nfa_stack, "bazfoobar"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foo"));
        try std.testing.expect(try nfa.match(&nfa_stack, "fo"));
        try std.testing.expect(try nfa.match(&nfa_stack, "fooooo"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "f"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bar"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "foo+");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "foobar"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
        try std.testing.expect(try nfa.match(&nfa_stack, "bazfoobar"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foo"));
        try std.testing.expect(try nfa.match(&nfa_stack, "fooo"));
        try std.testing.expect(try nfa.match(&nfa_stack, "fooooo"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "f"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bar"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
    }
}

test "repeating group" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "foo(bar)*baz");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "foobaz"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbarbarbaz"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bar"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "foo(bar)+baz");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbarbarbaz"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bar"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foobaz"));
    }
}

test "optionals" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "a?b");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "b"));
        try std.testing.expect(try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(try nfa.match(&nfa_stack, "ba"));
        try std.testing.expect(try nfa.match(&nfa_stack, "baba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "a"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "(foo)?bar");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "bar"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobar"));
        try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "baz"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foobaz"));
    }
}

test "anchors" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "^ab");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(try nfa.match(&nfa_stack, "abba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "ab$");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(try nfa.match(&nfa_stack, "baab"));
        try std.testing.expect(try nfa.match(&nfa_stack, "bab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "aba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }

    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "^a^b");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(!try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "abba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "a$b$");
        const nfa_result = try regex.buildNFA();
        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(!nfa_result.errors_occured);

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(!try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "baab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "aba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }
}

test "errors" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "a\\");
        const nfa_result = try regex.buildNFA();
        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(nfa_result.errors_occured);
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "(foo");
        const nfa_result = try regex.buildNFA();
        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(nfa_result.errors_occured);
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "foo)");
        const nfa_result = try regex.buildNFA();
        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(nfa_result.errors_occured);
    }
    {
        var errors = Errors.init(allocator, arena);
        defer errors.deinit();

        var regex = Self.init(allocator, &errors, "((foo)))");
        const nfa_result = try regex.buildNFA();
        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(nfa_result.errors_occured == (errors.count() != 0));
        try std.testing.expect(nfa_result.errors_occured);
    }
}
