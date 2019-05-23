#!/usr/bin/python3
import os

SERVER = "retrobox"
USER = "adam"


def pull_dirs():
    TMP = 'rsync -avz -e "ssh" --progress %s@%s:%s %s'
    DIRS = [
        ("/home/adam/music", "/ext2/music/"),
        ("/home/adam/gaming/roms", "/ext2/games/roms/"),
        ("/home/adam/gaming/saves", "/ext3/backups/gaming/saves/"),
        ("/home/adam/gaming/states", "/ext3/backups/gaming/states/"),
    ]

    for d in DIRS:
        print("-----------------------------------")
        print("Pulling: %s" % d[0])
        print("-----------------------------------")
        os.system(TMP % (USER, SERVER, d[1], d[0]))


def push_dirs():
    TMP = 'rsync -avz -e "ssh" --progress %s %s@%s:%s'
    DIRS = [
        ("/home/adam/music/", "/ext2/music"),
        ("/home/adam/gaming/roms/", "/ext2/games/roms"),
        ("/home/adam/gaming/saves/", "/ext3/backups/gaming/saves"),
        ("/home/adam/gaming/states/", "/ext3/backups/gaming/states"),
        ("/home/adam/code/", "/ext3/backups/ajrichpad/code"),
        ("/home/adam/docs/", "/ext3/backups/ajrichpad/docs"),
    ]

    for d in DIRS:
        print("-----------------------------------")
        print("Pushing: %s" % d[0])
        print("-----------------------------------")
        os.system(TMP % (d[0], USER, SERVER, d[1]))


if __name__ == "__main__":
    pull_dirs()
    push_dirs()
