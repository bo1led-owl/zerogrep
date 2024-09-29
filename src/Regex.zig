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
                '.' => RegexCharacter.Dot,
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
    Dot,
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
    var builder = NfaBuilder.init(gpa);

    errdefer {
        var nfa = builder.build();
        nfa.deinit(gpa);
    }

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
            .Dot => {
                const new_state = try builder.addState(.{});

                try builder.addTransition(cur_state, NFA.Transition{
                    .range = NFA.Transition.Range.fromRange(std.math.minInt(u8), std.math.maxInt(u8)),
                    .dest_index = new_state,
                });
                prev_state = cur_state;
                cur_state = new_state;
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
                    try added_ranges.append(gpa, Range{ .start = literal_char, .end = literal_char });
                }
            },
            .Erroneous => {},
            else => unreachable,
        }
    }

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

    try std.testing.expect(try nfa.match(&nfa_stack, "foobar") != null);
    try std.testing.expect(try nfa.match(&nfa_stack, "foobarbaz") != null);
    try std.testing.expect(try nfa.match(&nfa_stack, "bazfoobar") != null);
    try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "bar") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
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

    try std.testing.expect((try nfa.match(&nfa_stack, "foobar") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "bazfoobar") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "bar") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
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

    try std.testing.expect((try nfa.match(&nfa_stack, "foobar") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "bazfoobar") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "foo") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "bar") != null));
    try std.testing.expect((try nfa.match(&nfa_stack, "baz") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "random") != null));
    try std.testing.expect(!(try nfa.match(&nfa_stack, "word") != null));
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

        try std.testing.expect((try nfa.match(&nfa_stack, "foobar") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "bazfoobar") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foo") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "fo") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "fooooo") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "f") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bar") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
    }
    {
        var regex = Self.init("foo+");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect((try nfa.match(&nfa_stack, "foobar") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "bazfoobar") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foo") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "fooo") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "fooooo") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "f") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bar") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
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

        try std.testing.expect((try nfa.match(&nfa_stack, "foobaz") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbarbarbaz") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bar") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
    }
    {
        var regex = Self.init("foo(bar)+baz");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbarbarbaz") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bar") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foobaz") != null));
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

        try std.testing.expect((try nfa.match(&nfa_stack, "b") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "ab") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "ba") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "baba") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "a") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
    }
    {
        var regex = Self.init("(foo)?bar");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect((try nfa.match(&nfa_stack, "bar") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobar") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "foobarbaz") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "baz") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foobaz") != null));
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

        try std.testing.expect((try nfa.match(&nfa_stack, "ab") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "abba") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
    }
    {
        var regex = Self.init("ab$");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect((try nfa.match(&nfa_stack, "ab") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "baab") != null));
        try std.testing.expect((try nfa.match(&nfa_stack, "bab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "aba") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
    }

    {
        var regex = Self.init("^a^b");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(!(try nfa.match(&nfa_stack, "ab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "abba") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
    }
    {
        var regex = Self.init("a$b$");
        var nfa_result = try regex.buildNFA(allocator, arena);
        try std.testing.expect(nfa_result.errors.count() == 0);

        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(!(try nfa.match(&nfa_stack, "ab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "baab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "bab") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "aba") != null));
        try std.testing.expect(!(try nfa.match(&nfa_stack, "foo") != null));
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
            try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for (std.math.minInt(u8)..'a') |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for ('z' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
    }
    {
        var regex = Self.init(".");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect(nfa_result.errors.count() == 0);

        for (std.math.minInt(u8)..std.math.maxInt(u8)) |c| {
            try std.testing.expect(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null);
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
            try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for (std.math.minInt(u8)..'A') |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for ('Z' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
                try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
            } else {
                try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
                try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
            } else {
                try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
            try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for (std.math.minInt(u8)..'a') |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for ('f' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for (std.math.minInt(u8)..'a') |c| {
            try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for ('f' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
                try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
            } else {
                try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
                try std.testing.expect((try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
            } else {
                try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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

        try std.testing.expect((try nfa.match(&nfa_stack, "[") != null));
        for (std.math.minInt(u8)..'[') |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for (']' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
    }
    {
        var regex = Self.init("[]]");
        var nfa_result = try regex.buildNFA(allocator, arena);
        defer nfa_result.errors.deinit();

        try std.testing.expect(nfa_result.errors.count() == 0);

        var nfa = nfa_result.automata;
        defer nfa.deinit(allocator);

        try std.testing.expect((try nfa.match(&nfa_stack, "]") != null));
        for (std.math.minInt(u8)..']') |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
        }
        for (']' + 1..std.math.maxInt(u8)) |c| {
            try std.testing.expect(!(try nfa.match(&nfa_stack, &[1]u8{@intCast(c)}) != null));
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
