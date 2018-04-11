#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import sys
import os.path
import subprocess

GC_LOC = "/home/taylor/gc"


class BuildException(Exception):
    def __init__(self, process, stdout):
        self.process = process
        self.stdout = stdout.decode("utf-8")
    def __str__(self):
        return "Error in {} build:\n{}".format(self.process, self.stdout)


def make():
    print("Building compiler...", end=" ")
    sys.stdout.flush()
    subprocess.run(["make"], stdout=subprocess.DEVNULL, check=True)
    print("done.")


def build_c(filename, out_name=None):
    assert os.path.exists(filename), "source file {} doesn't exist".format(
        filename)
    if out_name is None:
        out_name, _ = os.path.splitext(filename)

    comp = subprocess.run(["./main.native", filename, "-o", out_name],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    if comp.returncode != 0:
        raise BuildException("ml", comp.stderr)

    return out_name + ".c"


def comp_c(filename,
           out_name=None,
           compiler="gcc",
           opt_level=0,
           debug=False,
           alloc_mode=True):
    assert os.path.exists(filename), "source file {} doesn't exist".format(
        filename)
    if out_name is None:
        out_name, _ = os.path.splitext(filename)

    gcc_args = [compiler, filename, "-Iruntime"]
    gcc_args += ["-I{}/include".format(GC_LOC), "{}/lib/libgc.a".format(GC_LOC)]
    gcc_args.append("-O{}".format(opt_level))
    gcc_args.append("-lm")
    gcc_args += ["-o", out_name]

    if debug:
        gcc_args.append("-g")

    if alloc_mode:
        gcc_args += ["-D" "DEBUG"]

    comp = subprocess.run(
        gcc_args, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if comp.returncode != 0:
        raise BuildException(compiler, comp.stderr)

    return out_name


def comp_ocaml(filename, out_name=None, compiler="ocamlopt"):
    assert os.path.exists(filename), "source file {} doesn't exist".format(
        filename)
    if out_name is None:
        out_name, _ = os.path.splitext(filename)

    comp = subprocess.run([compiler, filename, "-o", out_name],
                          stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
    if comp.returncode != 0:
        raise BuildException(compiler, comp.stderr)

    return out_name


def quick_build(filename, alloc_mode=True):
    return comp_c(build_c(filename), alloc_mode=alloc_mode)

