#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys
import os.path
import subprocess
import difflib


class BuildException(Exception):
    def __init__(self, process, stdout):
        self.process = process
        self.stdout = stdout.decode("utf-8")
    def __str__(self):
        return "Error in {} build:\n{}".format(self.process, self.stdout)


class UnexpectedOutputException(Exception):
    def __init__(self, output, expected):
        self.output = output.decode("utf-8")
        self.expected = expected.decode("utf-8")

    def __str__(self):
        differ = difflib.Differ()
        return "Diff:\n" + "\n".join(differ.compare(
            self.expected.splitlines(), self.output.splitlines()))


def make():
    print("Building compiler...", end=" ")
    sys.stdout.flush()
    subprocess.run(["make"], stdout=subprocess.DEVNULL, check=True)
    print("done.")


def build_file(filename, opt_level=0, debug=False):
    assert os.path.exists(filename), "source file {} doesn't exist".format(
        filename)
    out_name, _ = os.path.splitext(filename)

    comp = subprocess.run(["./main.native", filename, "-o", out_name],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    if comp.returncode != 0:
        raise BuildException("ml", comp.stderr)

    gcc_args = ["gcc", out_name + ".c", "-Iruntime", "-o", out_name]
    gcc_args.append("-O{}".format(opt_level))
    if debug:
        gcc_args.append("-g")

    comp = subprocess.run(
        gcc_args, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if comp.returncode != 0:
        raise BuildException("gcc", comp.stderr)

    return out_name


def run_regression_tests():
    num_tests = 0
    failures = []

    for test_name in os.listdir("tests/regression"):
        dir_name = "tests/regression/" + test_name
        if os.path.isdir(dir_name):
            num_tests += 1
            sys.stdout.flush()

            try:
                try:
                    expected = open(dir_name + "/expect.txt", "rb").read()
                except:
                    raise Exception(
                        "{} not present".format(dir_name + "/expect.txt"))

                print("Running test {}...".format(test_name), end=" ")
                exe_name = build_file(dir_name + "/test.ml")

                completed = subprocess.run(
                    [exe_name], check=True,
                    stdout=subprocess.PIPE, stderr=subprocess.PIPE)

                if completed.stdout != expected:
                    raise UnexpectedOutputException(completed.stdout, expected)
            except Exception as e:
                print("Failed!")
                failures.append(test_name)
                print(e)
            else:
                print("Passed!")

    num_passed = num_tests - len(failures)
    print("Summary: {}/{} tests passed".format(num_passed, num_tests))
    if not failures:
        print("No failed tests :)")
    else:
        print("Tests failed:")
        print("\t" + " ".join(failures))


def main():
    make()
    run_regression_tests()


if __name__ == "__main__":
    main()
