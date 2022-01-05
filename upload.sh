#!/bin/sh

PASS_REPO=~/.password-store
SECRET_FILE=sourcehut-pages-pat.gpg
TOKEN=`gpg2 -q --for-your-eyes-only --no-tty -d "$PASS_REPO/$SECRET_FILE"`
USERNAME=thales17

tar -C public -cvz . > site.tar.gz

curl --oauth2-bearer "$TOKEN" \
     -Fcontent=@site.tar.gz \
     https://pages.sr.ht/publish/$USERNAME.srht.site
