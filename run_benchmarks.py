#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys
import os.path
import subprocess
import build
import re
import pickle


def run_benchmarks(target=None, alloc_mode=True):
    results = {}

    for test_name in os.listdir("tests/benchmark"):
        if target is not None and target != test_name:
            continue
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
                times["ocamlc"] = time_executable(byte)
                times["gcc"] = time_executable(gcc)
                times["clang"] = time_executable(clang)

                results[test_name] = times

            except Exception as e:
                print("Error building {}".format(test_name))
                print(e)

    return results


def time_executable(exe_name, runs=50):
    try:
        times = []
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

            times.append(time)

        print()
        print("{} runs completed in {} seconds.".format(runs, sum(times)))
        return times
    except subprocess.CalledProcessError as e:
        print("Executable returned nonzero exit code, aborting")
        print(e)
    except Exception as e:
        print("Unexpected error occurred during timing, aborting")
        print(e)


def main():
    build.make()
    target = None
    if len(sys.argv) >= 2:
        target = sys.argv[-1]
    results = run_benchmarks(target=target, alloc_mode=False)
    pickle.dump(results, open("results.pickle", "wb+"))

if __name__ == "__main__":
    main()
