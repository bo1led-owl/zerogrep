from subprocess import Popen, PIPE
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


def runCmd(cmd: str) -> (str, str):
    p = Popen(cmd, shell=True, stdin=PIPE,
              stdout=PIPE, stderr=PIPE, close_fds=True)
    return (p.stdout.read().decode(), p.stderr.read().decode())


class Test:
    def __init__(self, name, cmd, expected):
        self.name = name
        self.cmd = cmd
        self.expected = expected
        self.success = True

    def run(self):
        print(f"{self.name}:")
        expected_stdout, expected_stderr = self.expected
        actual_stdout, actual_stderr = runCmd(self.cmd)

        self.expectEqual("stdout", expected_stdout, actual_stdout)
        self.expectEqual("stderr", expected_stderr, actual_stderr)
        if self.success:
            printColored(ANSI.OK, "\tOK")
        else:
            printColored(ANSI.FAIL, "\tFAILURE")

    def expectEqual(self, stream_name: str, expected: str, actual: str):
        expected_lines = expected.splitlines()
        actual_lines = actual.splitlines()
        for i in range(min(len(expected_lines), len(actual_lines))):
            if expected_lines[i] != actual_lines[i]:
                print((f"\t{stream_name}: line {i + 1}: "
                       f"expected `{expected_lines[i]}`, but got "
                       f"`{actual_lines[i]}`")
                      )
                self.success = False
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
            self.success = False
