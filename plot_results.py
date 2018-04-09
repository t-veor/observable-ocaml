#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import numpy as np
import matplotlib.pyplot as plt
import pickle

LABELS = [
    ("ocamlopt", "ocamlopt"),
    ("gcc", "gcc -O3"),
    ("clang", "clang -O3"),
]

BAR_WIDTH = 0.25
SPACING = 1.0


def main():
    data = pickle.load(open("results.pickle", "rb"))

    tests = sorted(data.keys())

    base_coords = np.arange(len(tests)) * SPACING

    coords = {}
    heights = {}
    for name, _ in LABELS:
        coords[name] = base_coords.copy()
        heights[name] = [data[test].get(name, 0.) for test in tests]
        heights[name] = [i if i is not None else 0. for i in heights[name]]
        heights[name] = np.asarray(heights[name])

    """
    # normalize heights to ocamlopt
    ocamlopt = heights["ocamlopt"].copy()
    for i in heights:
        heights[i] /= ocamlopt
    """

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

    rects = {}
    for name, label in LABELS:
        rects[name] = plt.bar(coords[name], heights[name], BAR_WIDTH, zorder=3)

    plt.grid(b=True, axis="y", linestyle="--", zorder=0)

    plt.xticks(base_coords, tests)
    plt.xlabel("Averaged execution time (s)")
    plt.title("Benchmark execution times")

    plt.legend([rects[name][0] for name, _ in LABELS],
               [label for _, label in LABELS])

    plt.show()


if __name__ == "__main__":
    main()
