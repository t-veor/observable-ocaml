#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys
import os.path
import subprocess
import difflib
import build


class UnexpectedOutputException(Exception):
    def __init__(self, output, expected):
        self.output = output.decode("utf-8")
        self.expected = expected.decode("utf-8")

    def __str__(self):
        differ = difflib.Differ()
        return "Diff:\n" + "\n".join(differ.compare(
            self.expected.splitlines(), self.output.splitlines()))


def run_regression_tests():
    num_tests = 0
    failures = []

    # run twice, once with alloc mode on and once without

    for alloc_mode in range(2):
        for test_name in os.listdir("tests/regression"):
            dir_name = "tests/regression/" + test_name
            if os.path.isdir(dir_name):
                num_tests += 1
                sys.stdout.flush()

                if not alloc_mode:
                    test_name += "(no-alloc)"

                try:
                    try:
                        expected = open(dir_name + "/expect.txt", "rb").read()
                    except:
                        raise Exception(
                            "{} not present".format(dir_name + "/expect.txt"))

                    print("Running test {}...".format(test_name), end=" ")
                    exe_name = build.quick_build(dir_name + "/test.ml",
                                                 alloc_mode=alloc_mode)

                    completed = subprocess.run(
                        [exe_name], check=True, timeout=1,
                        stdout=subprocess.PIPE, stderr=subprocess.PIPE)

                    if completed.stdout != expected:
                        raise UnexpectedOutputException(completed.stdout,
                                                        expected)
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
    build.make()
    run_regression_tests()


if __name__ == "__main__":
    main()
