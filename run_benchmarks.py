#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys
import os.path
import subprocess
import build
import re


def run_benchmarks(alloc_mode=True):
    results = {}

    for test_name in os.listdir("tests/benchmark"):
        times = {}
        dir_name = "tests/benchmark/" + test_name
        if os.path.isdir(dir_name):
            sys.stdout.flush()

            try:
                print("Building files...")
                c_file = build.build_c(dir_name + "/test.ml")

                asm = build.comp_ocaml(dir_name + "/test_std.ml",
                                       out_name=dir_name + "/test_asm",
                                       compiler="ocamlopt")
                byte = build.comp_ocaml(dir_name + "/test_std.ml",
                                        out_name=dir_name + "/test_byte",
                                        compiler="ocamlc")
                gcc = build.comp_c(c_file, out_name=dir_name + "/test_gcc",
                                   compiler="gcc", opt_level=3,
                                   alloc_mode=alloc_mode)
                clang = build.comp_c(c_file, out_name=dir_name + "/test_clang",
                                     compiler="clang", opt_level=3,
                                     alloc_mode=alloc_mode)

                times["ocamlopt"] = time_executable(asm)
                # time_executable(byte)
                times["gcc"] = time_executable(gcc)
                times["clang"] = time_executable(clang)

                results[test_name] = times

            except Exception as e:
                print("Error building {}".format(test_name))
                print(e)

    return results


def time_executable(exe_name, runs=10):
    try:
        total_time = 0.
        print("Running {} for {} run(s)...".format(exe_name, runs))
        print("Run ", end="")
        for i in range(runs):
            print(i + 1, end=" ")
            sys.stdout.flush()
            call = subprocess.run(
                ["time", "-f", "%E", exe_name], check=True,
                stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)

            time_str = call.stderr.decode("utf-8")
            match = re.match(r"((\d+):)?(\d+):(\d+.\d+)", time_str)

            if match is None:
                raise Exception("Could not parse \"{}\"".format(time_str))

            time = 0.
            if match.group(2) is not None: # hours
                time += 3600 * int(match.group(2))
            time += 60 * int(match.group(3)) # minutes
            time += float(match.group(4)) # seconds

            total_time += time

        print()
        print("{} runs completed in {} seconds.".format(runs, total_time))
        return total_time / runs
    except subprocess.CalledProcessError as e:
        print("Executable returned nonzero exit code, aborting")
        print(e)
    except Exception as e:
        print("Unexpected error occurred during timing, aborting")
        print(e)


def main():
    build.make()
    run_benchmarks(alloc_mode=False)


if __name__ == "__main__":
    main()
