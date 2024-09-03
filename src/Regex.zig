const std = @import("std");
const NFA = @import("NFA.zig");
const NfaBuilder = @import("NfaBuilder.zig");
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

    pub fn reset(self: *Lexer) void {
        self.cur_index = 0;
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

fn handleCharError(errors: *Errors(SourceSpan), err: CharError, char_index: u32) !RegexCharacter {
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

pub fn toStringLiteral(self: *Self, gpa: std.mem.Allocator) !?[]const u8 {
    defer self.lexer.reset();

    var len: u32 = 0;
    while (true) {
        switch (self.lexer.getChar() catch RegexCharacter.Erroneous) {
            .EOF => break,
            .Literal => len += 1,
            else => return null,
        }
    }

    self.lexer.reset();
    var result = try gpa.alloc(u8, len);
    for (0..len) |i| {
        switch (self.lexer.getChar() catch .Erroneous) {
            .Literal => |c| result[i] = c,
            else => unreachable,
        }
    }
    return result;
}

pub const AutomataBuildResult = struct {
    automata: NFA,
    errors: Errors(SourceSpan),
};

pub fn buildNFA(self: *Self, gpa: std.mem.Allocator, arena: std.mem.Allocator) !AutomataBuildResult {
    var errors = Errors(SourceSpan).init(gpa, arena);
    // var nfa = NFA.init(gpa);
    var builder = NfaBuilder.init(gpa);

    const cur_state = try builder.addState(.{});
    const res = try self.parse(gpa, &errors, &builder, cur_state, RegexCharacter.EOF);

    try builder.markStateAccepting(res.accepting_state);

    return .{
        .automata = builder.build(),
        .errors = errors,
    };
}

const ParseResult = struct {
    accepting_state: u32,
};

fn parse(self: *Self, gpa: std.mem.Allocator, errors: *Errors(SourceSpan), builder: *NfaBuilder, initial_state: u32, parse_until: RegexCharacter) !ParseResult {
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
        const regex_char = self.lexer.getChar() catch |err| try handleCharError(errors, err, self.lexer.cur_index - 1);
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
                builder.markAtLineStart(cur_state);
                parsed_atom = false;
            },
            .Dollar => {
                builder.markAtLineEnd(cur_state);
                parsed_atom = false;
            },
            .LBracket => {
                const new_state = try builder.addState(.{});
                try self.parseBracketExpr(gpa, errors, builder, cur_state, new_state);
                prev_state = cur_state;
                cur_state = new_state;
                parsed_atom = true;
            },
            .RBracket => {
                try errors.addError("Unmatched `]`", .{}, SourceSpan.fromChar(self.lexer.cur_index - 1));
            },
            .LParen => {
                const result = try self.parse(gpa, errors, builder, cur_state, RegexCharacter.RParen);
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

                const new_state = try builder.addState(.{});

                try builder.addEpsTransition(cur_state, prev_state);
                try builder.addEpsTransition(cur_state, new_state);
                try builder.addEpsTransition(prev_state, new_state);
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

                try builder.addEpsTransition(prev_state, cur_state);
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

                // var last_state: u32 = undefined;
                // for (prev_state..cur_state + 1) |state| {
                //     last_state = try builder.addState(try nf.states.items[state].cloneWithoutAnchors(gpa));
                // }
                const last_state = try builder.cloneRange(prev_state, cur_state);
                try builder.addEpsTransition(cur_state, last_state);
                try builder.addEpsTransition(cur_state, cur_state + 1);
                prev_state = cur_state;
                cur_state = last_state;
                parsed_atom = false;
            },
            .Literal => |literal_char| {
                const new_state = try builder.addState(.{});

                try builder.addTransition(cur_state, NFA.Transition{
                    .range = NFA.Transition.Range.fromChar(literal_char),
                    .dest_index = new_state,
                });
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
        cur_state = try builder.addState(.{});

        for (branches.items) |state| {
            try builder.addEpsTransition(state, cur_state);
        }
        branches.deinit(gpa);
    }

    return .{
        .accepting_state = cur_state,
    };
}

fn parseBracketExpr(self: *Self, gpa: std.mem.Allocator, errors: *Errors(SourceSpan), builder: *NfaBuilder, cur_state: u32, new_state: u32) !void {
    const Range = NFA.Transition.Range;
    var added_ranges = std.ArrayListUnmanaged(Range){};
    defer added_ranges.deinit(gpa);

    var cur_range_start: ?u8 = null;
    var first_char = true;
    var negate = false;

    while (true) {
        const regex_char = self.lexer.getChar() catch |err| try handleCharError(errors, err, self.lexer.cur_index - 1);

        if (first_char) {
            if (regex_char.is(RegexCharacter.Caret)) {
                negate = true;
                continue;
            }
            first_char = false;
        }

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
                if (added_ranges.items.len == 0) {
                    try added_ranges.append(gpa, Range{ .start = ']', .end = ']' });
                } else {
                    break;
                }
            },
            .Literal => |literal_char| {
                const next_char = self.lexer.peekChar() catch .Erroneous;

                if (cur_range_start == null and next_char.eq(RegexCharacter{ .Literal = '-' })) {
                    cur_range_start = literal_char;
                } else if (literal_char == '-' and cur_range_start != null) {
                    const start = cur_range_start.?;

                    if (!next_char.is(.Literal)) {
                        try added_ranges.append(gpa, Range{ .start = start, .end = start });
                        try added_ranges.append(gpa, Range{ .start = '-', .end = '-' });
                        continue;
                    }

                    const end = next_char.Literal;
                    if (end < start) {
                        try errors.addError(
                            "Range end `{c}` is less than start `{c}`",
                            .{ end, start },
                            .{ .start = self.lexer.cur_index - 2, .end = self.lexer.cur_index },
                        );
                    } else {
                        _ = self.lexer.getChar() catch unreachable;
                        try added_ranges.append(gpa, Range{ .start = start, .end = end });
                    }
                    cur_range_start = null;
                } else {
                    // try sortedInsert(gpa, &char_set, literal_char);
                    try added_ranges.append(gpa, Range{ .start = literal_char, .end = literal_char });
                }
            },
            .Erroneous => {},
            else => unreachable,
        }
    }

    // std.mem.sort(Range, added_ranges.items, {}, Range.lessThan);

    var charset = std.bit_set.ArrayBitSet(usize, 256).initEmpty();
    for (added_ranges.items) |r| {
        charset.setRangeValue(.{ .start = r.start, .end = r.end + 1 }, true);
    }

    if (negate) {
        charset.toggleAll();
    }

    cur_range_start = null;
    var cur_range_end: u8 = undefined;
    for (0..256) |i| {
        if (charset.isSet(i)) {
            if (cur_range_start == null) {
                cur_range_start = @as(u8, @intCast(i));
            }

            cur_range_end = @as(u8, @intCast(i));
        } else if (cur_range_start) |start| {
            std.debug.assert(start <= cur_range_end);
            try builder.addTransition(cur_state, NFA.Transition{
                .range = Range.fromRange(start, cur_range_end),
                .dest_index = new_state,
            });
            cur_range_start = null;
        }
    }

    if (cur_range_start) |start| {
        std.debug.assert(start <= cur_range_end);
        try builder.addTransition(cur_state, NFA.Transition{
            .range = Range.fromRange(start, cur_range_end),
            .dest_index = new_state,
        });
    }
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
    defer nfa.deinit(allocator);

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
    defer nfa.deinit(allocator);

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
    defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa.deinit(allocator);

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
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

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
        var regex = Self.init("[A-Z]+");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

        for ('A'..'Z' + 1) |c| {
            try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for (std.math.minInt(u8)..'A') |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for ('Z' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
    }
    {
        var regex = Self.init("[a-zA-Z]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

        for (std.math.minInt(u8)..std.math.maxInt(u8)) |c| {
            if ('a' <= c and c <= 'z' or 'A' <= c and c <= 'Z') {
                try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            } else {
                try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            }
        }
    }
    {
        var regex = Self.init("[^a-zA-Z]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

        for (std.math.minInt(u8)..std.math.maxInt(u8)) |c| {
            if ('a' <= c and c <= 'z' or 'A' <= c and c <= 'Z') {
                try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            } else {
                try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
            }
        }
    }
    {
        var regex = Self.init("[badcef]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

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
        var regex = Self.init("[^badcef]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

        for ('a'..'f' + 1) |c| {
            try std.testing.expect(!try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for (std.math.minInt(u8)..'a') |c| {
            try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
        for ('f' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}));
        }
    }
    {
        var regex = Self.init("[a-]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

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
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

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
        defer nfa_result.errors.deinit();

        try std.testing.expect(nfa_result.errors.count() == 0);

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

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
        defer nfa_result.errors.deinit();

        try std.testing.expect(nfa_result.errors.count() == 0);

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

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

        defer nfa_result.automata.deinit(allocator);
        defer nfa_result.errors.deinit();

        try std.testing.expect(nfa_result.errors.count() != 0);
    }
}
