#!/usr/bin/python3
import os

SERVER = "retrobox"
USER = "adam"

SYNC_DIRS = [
    ("/home/adam/music/", "/ext2/music"),
    ("/home/adam/gaming/roms/", "/ext2/games/roms"),
    ("/home/adam/gaming/saves/", "/ext3/backups/gaming/saves"),
    ("/home/adam/gaming/states/", "/ext3/backups/gaming/states"),
]

BACKUP_DIRS = [
    ("/home/adam/music", "/ext2/music/"),
    ("/home/adam/gaming/roms", "/ext2/games/roms/"),
    ("/home/adam/gaming/saves", "/ext3/backups/gaming/saves/"),
    ("/home/adam/gaming/states", "/ext3/backups/gaming/states/"),
    ("/home/adam/code", "/ext3/backups/ajrichpad/code/"),
    ("/home/adam/docs", "/ext3/backups/ajrichpad/docs/"),
]

SYNC_TMP = 'rsync -avz -e "ssh" --progress %s %s@%s:%s'
BACKUP_TMP = 'rsync -avz -e "ssh" --progress %s@%s:%s %s'


def pull_dir(d):
    print("-----------------------------------")
    print("Pulling: %s" % d[0])
    print("-----------------------------------")
    os.system(SYNC_TMP % (d[0], USER, SERVER, d[1]))


def push_dir(d):
    print("-----------------------------------")
    print("Pushing: %s" % d[0])
    print("-----------------------------------")
    os.system(BACKUP_TMP % (USER, SERVER, d[1], d[0]))


if __name__ == "__main__":
    for d in SYNC_DIRS:
        pull_dir(d)

    for d in BACKUP_DIRS:
        push_dir(d)
