#!/usr/bin/env python3

import os
import sys


def get_dirs(path):
    dirs = []
    for d in os.listdir(path):
        if os.path.isdir(d):
            dirs.append(os.path.join(path, d))
    return dirs


def make_cbz(path):
    os.system('zip -r "%s.cbz" "%s"' % (path, path))


if __name__ == "__main__":
    path = "."
    if len(sys.argv) > 1:
        path = sys.argv[1]

    for d in get_dirs(path):
        make_cbz(d)
