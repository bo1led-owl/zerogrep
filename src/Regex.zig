const std = @import("std");
const NFA = @import("NFA.zig");
const Errors = @import("errors.zig").Errors;

const Self = @This();

lexer: Lexer,

pub fn init(pattern: []const u8) Self {
    return .{
        .lexer = .{
            .input = pattern,
        },
    };
}

const CharError = error{
    TrailingBackslash,
    UnknownEscapeSequence,
};

const Lexer = struct {
    input: []const u8,
    cur_index: u32 = 0,

    fn advance(self: *Lexer) ?u8 {
        if (self.cur_index >= self.input.len) return null;

        defer self.cur_index += 1;
        return self.input[self.cur_index];
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
                '[' => RegexCharacter.LBracket,
                ']' => RegexCharacter.RBracket,
                '|' => RegexCharacter.Pipe,
                '^' => RegexCharacter.Caret,
                '$' => RegexCharacter.Dollar,
                else => |c| RegexCharacter{ .Literal = c },
            };
        } else {
            return switch (cur_char.?) {
                '(', ')', '|', '*', '+', '?', '^', '$', '[', ']' => |c| RegexCharacter{ .Literal = c },
                else => CharError.UnknownEscapeSequence,
            };
        }
    }

    pub fn peekChar(self: Lexer) CharError!RegexCharacter {
        var copy = self;
        return copy.getChar();
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

    pub fn is(self: RegexCharacter, kind: std.meta.Tag(RegexCharacter)) bool {
        const tag = std.meta.Tag(RegexCharacter);
        return @as(tag, self) == kind;
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

pub const SourceSpan = struct {
    start: u32,
    end: u32,

    pub fn fromChar(i: u32) SourceSpan {
        return .{ .start = i, .end = i };
    }

    pub fn fromRange(start: u32, end: u32) SourceSpan {
        return .{ .start = start, .end = end };
    }
};

fn handleError(errors: *Errors(SourceSpan), err: CharError, char_index: u32) !RegexCharacter {
    switch (err) {
        CharError.TrailingBackslash => {
            try errors.addError("Trailing backslash", .{}, SourceSpan.fromChar(char_index));
            return RegexCharacter.Erroneous;
        },
        CharError.UnknownEscapeSequence => {
            try errors.addError("Unknown escape sequence", .{}, SourceSpan.fromRange(char_index - 1, char_index));
            return RegexCharacter.Erroneous;
        },
    }
}

pub const AutomataBuildResult = struct {
    automata: NFA,
    errors: Errors(SourceSpan),
};

pub fn buildNFA(self: *Self, gpa: std.mem.Allocator, arena: std.mem.Allocator) !AutomataBuildResult {
    var errors = Errors(SourceSpan).init(gpa, arena);
    var nfa = NFA.init(gpa);

    const cur_state = try nfa.addState(.{});
    const res = try self.parse(gpa, &errors, &nfa, cur_state, RegexCharacter.EOF);

    nfa.setAcceptingState(res.accepting_state);

    return .{
        .automata = nfa,
        .errors = errors,
    };
}

const ParseResult = struct {
    accepting_state: u32,
};

fn parse(self: *Self, gpa: std.mem.Allocator, errors: *Errors(SourceSpan), nfa: *NFA, initial_state: u32, parse_until: RegexCharacter) !ParseResult {
    std.debug.assert(switch (parse_until) {
        .EOF => true,
        .RParen => true,
        else => false,
    });

    var branches = std.ArrayListUnmanaged(u32){}; // indices of branches ends

    var cur_state = initial_state;
    var prev_state: u32 = undefined;
    var parsed_atom = false;
    var finished_correctly = false;
    while (true) {
        const regex_char = self.lexer.getChar() catch |err| try handleError(errors, err, self.lexer.cur_index - 1);
        if (regex_char.eq(parse_until)) {
            finished_correctly = true;
            if (branches.items.len > 0) {
                try branches.append(gpa, cur_state);
            }
            break;
        }

        switch (regex_char) {
            .EOF => break,
            .Caret => {
                nfa.markAtLineStart(cur_state);
                parsed_atom = false;
            },
            .Dollar => {
                nfa.markAtLineEnd(cur_state);
                parsed_atom = false;
            },
            .LBracket => {
                const new_state = try nfa.addState(.{});
                try self.parseBracketExpr(gpa, errors, nfa, cur_state, new_state);
                prev_state = cur_state;
                cur_state = new_state;
                parsed_atom = true;
            },
            .RBracket => {
                try errors.addError("Unmatched `]`", .{}, SourceSpan.fromChar(self.lexer.cur_index - 1));
            },
            .LParen => {
                const result = try self.parse(gpa, errors, nfa, cur_state, RegexCharacter.RParen);
                prev_state = cur_state;
                cur_state = result.accepting_state;
                parsed_atom = true;
            },
            .RParen => {
                try errors.addError("Unmatched `)`", .{}, SourceSpan.fromChar(self.lexer.cur_index - 1));
                parsed_atom = false;
            },
            .Pipe => {
                try branches.append(gpa, cur_state);
                prev_state = cur_state;
                cur_state = initial_state;
                parsed_atom = false;
            },
            .Asterisk => {
                if (!parsed_atom) {
                    try errors.addError(
                        "The preceding token is not quantifiable",
                        .{},
                        SourceSpan.fromChar(self.lexer.cur_index - 1),
                    );
                    continue;
                }

                const new_state = try nfa.addState(.{});

                try nfa.addEpsTransition(cur_state, prev_state);
                try nfa.addEpsTransition(cur_state, new_state);
                try nfa.addEpsTransition(prev_state, new_state);
                prev_state = cur_state;
                cur_state = new_state;
                parsed_atom = false;
            },
            .QuestionMark => {
                if (!parsed_atom) {
                    try errors.addError(
                        "The preceding token is not quantifiable",
                        .{},
                        SourceSpan.fromChar(self.lexer.cur_index - 1),
                    );
                    continue;
                }

                try nfa.addEpsTransition(prev_state, cur_state);
                prev_state = cur_state;
                parsed_atom = false;
            },
            .Plus => {
                if (!parsed_atom) {
                    try errors.addError(
                        "The preceding token is not quantifiable",
                        .{},
                        SourceSpan.fromChar(self.lexer.cur_index - 1),
                    );
                    continue;
                }

                var last_state: u32 = undefined;
                for (prev_state..cur_state + 1) |state| {
                    last_state = try nfa.addState(try nfa.states.items[state].cloneWithoutAnchors(gpa));
                }
                try nfa.addEpsTransition(cur_state, last_state);
                try nfa.addEpsTransition(cur_state, cur_state + 1);
                prev_state = cur_state;
                cur_state = last_state;
                parsed_atom = false;
            },
            .Literal => |literal_char| {
                const new_state = try nfa.addState(.{});

                try nfa.addTransition(cur_state, NFA.Transition.fromChar(literal_char, new_state));
                prev_state = cur_state;
                cur_state = new_state;
                parsed_atom = true;
            },
            .Erroneous => {},
        }
    }

    if (!finished_correctly) {
        switch (parse_until) {
            .RParen => {
                const i = std.mem.lastIndexOfScalar(u8, self.lexer.input[0..self.lexer.cur_index], '(').?;
                try errors.addError("Unmatched `(`", .{}, SourceSpan.fromChar(@intCast(i)));
            },
            else => unreachable,
        }
    }

    if (branches.items.len > 0) {
        cur_state = try nfa.addState(.{});

        for (branches.items) |state| {
            try nfa.addEpsTransition(state, cur_state);
        }
        branches.deinit(gpa);
    }

    return .{
        .accepting_state = cur_state,
    };
}

fn parseBracketExpr(self: *Self, gpa: std.mem.Allocator, errors: *Errors(SourceSpan), nfa: *NFA, cur_state: u32, new_state: u32) !void {
    var char_set = std.ArrayListUnmanaged(u8){};
    defer char_set.deinit(gpa);

    var cur_range_bot: ?u8 = null;
    var added_ranges: u32 = 0;

    while (true) {
        const regex_char = self.lexer.getChar() catch |err| try handleError(errors, err, self.lexer.cur_index - 1);

        const bracket_expr_char = switch (regex_char) {
            .Caret => RegexCharacter{ .Literal = '^' },
            .Dollar => RegexCharacter{ .Literal = '$' },
            .LBracket => RegexCharacter{ .Literal = '[' },
            .LParen => RegexCharacter{ .Literal = '(' },
            .RParen => RegexCharacter{ .Literal = ')' },
            .Pipe => RegexCharacter{ .Literal = '|' },
            .Asterisk => RegexCharacter{ .Literal = '*' },
            .Plus => RegexCharacter{ .Literal = '+' },
            .QuestionMark => RegexCharacter{ .Literal = '?' },
            else => regex_char,
        };

        switch (bracket_expr_char) {
            .EOF => {
                const i = std.mem.lastIndexOfScalar(u8, self.lexer.input[0..self.lexer.cur_index], '[').?;
                try errors.addError("Unmatched `[`", .{}, SourceSpan.fromChar(@intCast(i)));
                break;
            },
            .RBracket => {
                if (char_set.items.len == 0 and added_ranges == 0) {
                    try char_set.append(gpa, ']');
                } else {
                    break;
                }
            },
            .Literal => |literal_char| {
                const next_char = self.lexer.peekChar() catch .Erroneous;

                if (cur_range_bot == null and next_char.eq(RegexCharacter{ .Literal = '-' })) {
                    cur_range_bot = literal_char;
                } else if (literal_char == '-' and cur_range_bot != null) {
                    const c = cur_range_bot.?;

                    if (!next_char.is(.Literal)) {
                        try sortedInsert(gpa, &char_set, c);
                        try sortedInsert(gpa, &char_set, '-');
                        continue;
                    }

                    const l = next_char.Literal;
                    if (l < c) {
                        try errors.addError(
                            "Range top `{c}` is less than bottom `{c}`",
                            .{ l, c },
                            .{ .start = self.lexer.cur_index - 2, .end = self.lexer.cur_index },
                        );
                    } else {
                        _ = self.lexer.getChar() catch unreachable;
                        try nfa.addTransition(cur_state, NFA.Transition.fromRange(c, l, new_state));
                        added_ranges += 1;
                    }
                    cur_range_bot = null;
                } else {
                    try sortedInsert(gpa, &char_set, literal_char);
                }
            },
            .Erroneous => {},
            else => unreachable,
        }
    }

    cur_range_bot = null;
    var cur_range_top: ?u8 = null;

    for (char_set.items) |c| {
        if (cur_range_bot == null) {
            cur_range_bot = c;
        }

        if (cur_range_top) |top| {
            if (c - top > 1) {
                try nfa.addTransition(cur_state, NFA.Transition.fromRange(cur_range_bot.?, top, new_state));
                cur_range_bot = c;
            }
        }
        cur_range_top = c;
    }

    if (cur_range_bot != null and cur_range_top != null) {
        try nfa.addTransition(cur_state, NFA.Transition.fromRange(cur_range_bot.?, cur_range_top.?, new_state));
    }
}

fn sortedInsert(allocator: std.mem.Allocator, items: *std.ArrayListUnmanaged(u8), x: u8) !void {
    const i = std.sort.lowerBound(u8, x, items.items, {}, std.sort.asc(u8));
    if (i < items.items.len and items.items[i] == x) {
        return;
    }

    try items.insert(allocator, i, x);
}

test "basic" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var regex = Self.init("foobar");
    var nfa_result = try regex.buildNFA(allocator, arena);
    try std.testing.expect(nfa_result.errors.count() == 0);

    defer nfa_result.errors.deinit();

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

    var regex = Self.init("(fo(oba)r)");
    var nfa_result = try regex.buildNFA(allocator, arena);
    try std.testing.expect(nfa_result.errors.count() == 0);

    defer nfa_result.errors.deinit();

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

    var regex = Self.init("foo|bar|baz");
    var nfa_result = try regex.buildNFA(allocator, arena);
    try std.testing.expect(nfa_result.errors.count() == 0);

    defer nfa_result.errors.deinit();

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
        var regex = Self.init("foo*");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("foo+");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("foo(bar)*baz");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("foo(bar)+baz");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("a?b");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("(foo)?bar");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("^ab");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(try nfa.match(&nfa_stack, "abba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }
    {
        var regex = Self.init("ab$");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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
        var regex = Self.init("^a^b");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(!try nfa.match(&nfa_stack, "ab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "abba"));
        try std.testing.expect(!try nfa.match(&nfa_stack, ""));
        try std.testing.expect(!try nfa.match(&nfa_stack, "bab"));
        try std.testing.expect(!try nfa.match(&nfa_stack, "foo"));
    }
    {
        var regex = Self.init("a$b$");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

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

test "bracket expressions" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    var nfa_stack = NFA.Stack.init(allocator);
    defer nfa_stack.deinit();

    {
        var regex = Self.init("[a-z]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        for ('a'..'z' + 1) |c| {
            try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for (std.math.minInt(u8)..'a') |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for ('z' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
    }
    {
        var regex = Self.init("[a-zA-Z]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        for (std.math.minInt(u8)..std.math.maxInt(u8)) |c| {
            if ('a' <= c and c <= 'z' or 'A' <= c and c <= 'Z') {
                try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            } else {
                try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            }
        }
    }
    {
        var regex = Self.init("[badcef]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        for ('a'..'f' + 1) |c| {
            try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for (std.math.minInt(u8)..'a') |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for ('f' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
    }
    {
        var regex = Self.init("[a-]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        for (std.math.minInt(u8)..std.math.maxInt(u8)) |c| {
            if (c == 'a' or c == '-') {
                try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            } else {
                try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            }
        }
    }
    {
        var regex = Self.init("[-a]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        for (std.math.minInt(u8)..std.math.maxInt(u8)) |c| {
            if (c == 'a' or c == '-') {
                try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            } else {
                try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            }
        }
    }
    {
        var regex = Self.init("[[]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "["));
        for (std.math.minInt(u8)..'[') |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for (']' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
    }
    {
        var regex = Self.init("[]]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit();

        try std.testing.expect(try nfa.match(&nfa_stack, "]"));
        for (std.math.minInt(u8)..']') |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for (']' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
    }
}

test "errors" {
    const allocator = std.testing.allocator;
    var arena_allocator = std.heap.ArenaAllocator.init(allocator);
    const arena = arena_allocator.allocator();
    defer arena_allocator.deinit();

    const patterns = [_][]const u8{
        "a\\",
        "(",
        ")",
        "(foo",
        "foo)",
        "(()",
        "(()foo",
        "(foo))",
        "(*)",
        "*",
        "?",
        "+",
        "a(+)",
        "(foo)(?)",
        "[z-a]",
        "[",
        "]",
    };

    for (patterns) |pat| {
        var regex = Self.init(pat);
        var nfa_result = try regex.buildNFA(allocator, arena);

        defer nfa_result.automata.deinit();
        defer nfa_result.errors.deinit();

        try std.testing.expect(nfa_result.errors.count() != 0);
    }
}
