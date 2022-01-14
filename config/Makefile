linux: bash git xprofile nethack log-dir beets cdripping emacs utils suckless kitty tmux git bash xmodmap dunst

git:
	@touch ~/.gitconfig-user

xprofile:
	@touch ~/.xprofile-local
	@chmod +x ~/.xprofile-local
	@./tangle.sh xprofile.org

nethack:
	@./tangle.sh nethack.org

log-dir:
	@mkdir -p ~/.local/log

beets:
	@mkdir -p ~/.config/beets/data

kitty:
	@./tangle.sh kitty.org

tmux:
	@./tangle.sh tmux.org

git:
	@./tangle.sh git.org

bash:
	@touch ~/.bash_env
	@./tangle.sh bash.org

xmodmap:
	@./tangle.sh xmodmap.org

dunst:
	@./tangle.sh dunst.org

cdripping:
	@./tangle.sh cdripping.org

xinitrc:
	@./tangle.sh xinitrc.org

emacs:
	@touch ~/.emacs-custom.el
	@touch ~/.elfeed-feeds.el
	@./tangle.sh emacs.org

utils:
	@./tangle.sh utils.org

suckless:
	@./tangle.sh suckless.org

openbsd: emacs nethack kitty tmux git bash cdripping xinitrc
