from subprocess import Popen, PIPE
import sys
import pathlib


class ANSI:
    RESET = '\033[0m'
    OK = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    BOLD = '\033[1m'


def printColored(color: str, msg: str):
    print(color + msg + ANSI.RESET)


def getPathToTest(filename: str) -> str:
    parent_dir = pathlib.Path(__file__).parent.resolve().as_posix()
    return parent_dir + '/data/' + filename


def makeCmd(pattern: str, options: [str], paths: [str]) -> str:
    options = ' '.join(options)
    paths = ' '.join(map(getPathToTest, paths))

    return f"{options} \"{pattern}\" {paths}"


def runCmd(cmd: str) -> (str, str):
    p = Popen(cmd, shell=True, stdin=PIPE,
              stdout=PIPE, stderr=PIPE, close_fds=True)
    return (p.stdout.read().decode(), p.stderr.read().decode())


class Test:
    def __init__(
        self,
        name: str,
        pattern: str,
        options: [str],
        paths: [str],
        expected: (str, str)
    ):
        self.name = name
        self.cmd = makeCmd(pattern, options, paths)
        self.expected = expected

    def run(self, executable: str):
        print(f"{self.name}:")

        success = True
        expected_stdout, expected_stderr = self.expected
        actual_stdout, actual_stderr = runCmd(executable + ' ' + self.cmd)

        success = success and self.expectEqual(
            "stdout", expected_stdout, actual_stdout)
        success = success and self.expectEqual(
            "stderr", expected_stderr, actual_stderr)

        if success:
            printColored(ANSI.OK, "\tOK")
        else:
            printColored(ANSI.FAIL, "\tFAILURE")

    def expectEqual(
        self,
        stream_name: str,
        expected: str,
        actual: str
    ) -> bool:
        result = True

        expected_lines = expected.splitlines()
        actual_lines = actual.splitlines()
        for i in range(min(len(expected_lines), len(actual_lines))):
            if expected_lines[i] != actual_lines[i]:
                print(
                    (f"\t{stream_name}: line {i + 1}: "
                     f"expected `{expected_lines[i]}`, but got "
                     f"`{actual_lines[i]}`")
                )
                result = False
        line_count_diff = len(actual_lines) - len(expected_lines)

        if line_count_diff > 0:
            print(
                (f"\t{stream_name}: "
                 f"actual line count exceeds expected by {line_count_diff}:\n"
                 f"\t\t`{'\n\t\t'.join(actual_lines[len(expected_lines):])}`")
            )
        elif line_count_diff < 0:
            print(
                (f"\t{stream_name}: "
                 f"output is {-line_count_diff} lines shorter than expected:\n"
                 f"\t\t`{'\n\t\t'.join(expected_lines[len(actual_lines):])}`")
            )

        if line_count_diff != 0:
            result = False
        return result


class TestFramework:
    def __init__(self, tests: [Test]):
        self.tests = tests

    def run(self):
        argc = len(sys.argv)
        if argc < 2:
            print("Expected command as an argument")
            exit(2)
        elif argc > 2:
            print(
                ("Too many arguments passed, "
                 "only expected command as an argument")
            )
            exit(2)

        command = sys.argv[1]

        for t in self.tests:
            t.run(command)
