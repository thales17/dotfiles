linux: bash git xprofile nethack log-dir beets cdripping emacs utils suckless kitty tmux git bash xmodmap dunst

git:
	@touch ~/.gitconfig-user
	@../scripts/tangle.sh git.org

xprofile:
	@touch ~/.xprofile-local
	@chmod +x ~/.xprofile-local
	@../scripts/tangle.sh xprofile.org

nethack:
	@../scripts/tangle.sh nethack.org

log-dir:
	@mkdir -p ~/.local/log

beets:
	@mkdir -p ~/.config/beets/data

kitty:
	@../scripts/tangle.sh kitty.org

tmux:
	@../scripts/tangle.sh tmux.org

bash:
	@touch ~/.bash_env
	@../scripts/tangle.sh bash.org

xmodmap:
	@../scripts/tangle.sh xmodmap.org

dunst:
	@../scripts/tangle.sh dunst.org

cdripping:
	@../scripts/tangle.sh cdripping.org

xinitrc:
	@../scripts/tangle.sh xinitrc.org

emacs:
	@touch ~/.emacs-custom.el
	@touch ~/.elfeed-feeds.el
	@../scripts/tangle.sh emacs.org

utils:
	@../scripts/tangle.sh utils.org

suckless:
	@../scripts/tangle.sh suckless.org

openbsd: emacs nethack kitty tmux git bash cdripping xinitrc
