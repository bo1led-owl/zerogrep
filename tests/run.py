from framework import getPathToTest, runCmd, Test

executable = "./zig-out/bin/zg"


def makeCmd(pattern: str, options: [str], paths: [str]) -> str:
    options = ' '.join(options)
    paths = ' '.join(map(getPathToTest, paths))

    return f"{executable} {options} \"{pattern}\" {paths}"


tests = [
    Test(
        "Foo",
        makeCmd("foo", [], ["foo"]),
        ("foo\nfoo", "")
    ),
    Test(
        "No matches",
        makeCmd("fizz", [], ["foo"]),
        ("", "")
    ),
    Test(
        "Basic regex",
        makeCmd("foo|bar", [], ["foo"]),
        ("foo\nbar\nfoo\nbar", "")
    ),
    Test(
        "All flags",
        makeCmd("foo", ["-n", "-f", "--color=auto"], ["foo"]),
        (f"{getPathToTest("foo")}\n   1: foo\n   4: foo\n", "")
    ),
    Test(
        "All flags except line numbers",
        makeCmd("foo", ["-N", "-f", "--color=auto"], ["foo"]),
        (f"{getPathToTest("foo")}\nfoo\nfoo\n", "")
    ),
    Test(
        "All flags except filenames",
        makeCmd("foo", ["-n", "-F", "--color=auto"], ["foo"]),
        ("   1: foo\n   4: foo\n", "")
    ),
    Test(
        "Erroneous `color`",
        makeCmd("foo", ["-n", "-F", "--color=foo"], ["foo"]),
        (
            "",
            ("Error: Unknown value for `--color`: `foo`. "
             "Possible values are `auto`, `on` and `off`")
        )
    )
]

for t in tests:
    t.run()
