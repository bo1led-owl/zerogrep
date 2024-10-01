from framework import getPathToTest, runCmd, Test

executable = "./zig-out/bin/zg"


def makeCmd(pattern: str, paths: [str]) -> str:
    return f"{executable} \"{pattern}\" {' '.join(map(getPathToTest, paths))}"


tests = [
    Test(
        "Foo",
        makeCmd("foo", ["foo"]),
        ("foo\nfoo", "")
    ),
    Test(
        "No matches",
        makeCmd("fizz", ["foo"]),
        ("", "")
    ),
    Test(
        "Basic regex",
        makeCmd("foo|bar", ["foo"]),
        ("foo\nbar\nfoo\nbar", "")
    )
]

for t in tests:
    t.run()
