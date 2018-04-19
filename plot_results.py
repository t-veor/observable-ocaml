#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt
import pickle

LABELS = [
    ("ocamlopt", "ocamlopt"),
#    ("ocamlc", "ocamlc"),
    ("gcc", "gcc -O3 (reduced allocations)"),
    ("clang", "clang -O3 (reduced allocations)"),
]

BAR_WIDTH = 0.25
SPACING = 1.0


def main():
    data = pickle.load(open("results.pickle", "rb"))

    tests = sorted(data.keys())

    base_coords = np.arange(len(tests)) * SPACING

    coords = {}
    heights = {}
    errors = {}
    mean = lambda i: (sum(i) / len(i)) if i else 0.
    stddev = lambda i: ((mean([j * j for j in i]) - mean(i) ** 2) ** .5) if i else 0.
    for name, _ in LABELS:
        coords[name] = base_coords.copy()
        heights[name] = [mean(data[test].get(name, [])) for test in tests]
        heights[name] = np.asarray(heights[name])
        errors[name] = [stddev(data[test].get(name, [])) for test in tests]
        errors[name] = np.asarray(errors[name])

    # normalize heights to ocamlc
    ocamlc = np.asarray([mean(data[test].get("ocamlc", [])) for test in tests])
    for i in heights:
        heights[i] /= ocamlc
        errors[i] /= ocamlc

    for i, test in enumerate(tests):
        total = 0
        for name, _ in LABELS:
            if data[test].get(name, None) is not None:
                total += 1

        curr = -total / 2 + .5
        for name, _ in LABELS:
            if data[test].get(name, None) is not None:
                coords[name][i] = coords[name][i] + curr * BAR_WIDTH
                curr += 1

    plt.figure(figsize=(8, 6))

    rects = {}
    for name, label in LABELS:
        rects[name] = plt.bar(coords[name], heights[name], BAR_WIDTH, zorder=3)
        plt.errorbar(coords[name], heights[name], yerr=errors[name],
                     ecolor="k", fmt="none", capsize=2, zorder=4)

    plt.grid(b=True, axis="y", linestyle="--", zorder=0)

    plt.xticks(base_coords, tests, rotation=30, ha="right")
    plt.ylabel("Normalised execution time")

    plt.legend([rects[name][0] for name, _ in LABELS],
               [label for _, label in LABELS])

    plt.show()


if __name__ == "__main__":
    main()
