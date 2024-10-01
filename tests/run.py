from framework import getPathToTest, runCmd, Test

def makeCmd(pattern: str, paths: [str]) -> str:
    return f"./zig-out/bin/zg \"{pattern}\" {' '.join(map(getPathToTest, paths))}"


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
