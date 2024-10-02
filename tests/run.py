from framework import makeCmd, getPathToTest, TestFramework, Test

tests = [
    Test(
        name="Foo",
        pattern="foo",
        options=[],
        paths=["foo"],
        expected=("foo\nfoo", "")
    ),
    Test(
        name="No matches",
        pattern="fizz",
        options=[],
        paths=["foo"],
        expected=("", "")
    ),
    Test(
        name="Basic regex",
        pattern="foo|bar",
        options=[],
        paths=["foo"],
        expected=("foo\nbar\nfoo\nbar", "")
    ),
    Test(
        name="All flags",
        pattern="foo",
        options=["-n", "-f", "--color=auto"],
        paths=["foo"],
        expected=(f"{getPathToTest("foo")}\n   1: foo\n   4: foo\n", "")
    ),
    Test(
        name="All flags except line numbers",
        pattern="foo",
        options=["-N", "-f", "--color=auto"],
        paths=["foo"],
        expected=(f"{getPathToTest("foo")}\nfoo\nfoo\n", "")
    ),
    Test(
        name="All flags except filenames",
        pattern="foo",
        options=["-n", "-F", "--color=auto"],
        paths=["foo"],
        expected=("   1: foo\n   4: foo\n", "")
    ),
    Test(
        name="Erroneous `color`",
        pattern="foo",
        options=["-n", "-F", "--color=foo"],
        paths=["foo"],
        expected=(
            "",
            ("Error: Unknown value for `--color`: `foo`. "
             "Possible values are `auto`, `on` and `off`")
        )
    )
]

framework = TestFramework(tests)
framework.run()
