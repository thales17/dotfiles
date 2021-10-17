all: config emacs utils suckless

config:
	@touch ~/.bash_env
	@touch ~/.gitconfig-user
	@touch ~/.xprofile-local
	@chmod +x ~/.xprofile-local
	@./tangle.sh config.org

emacs:
	@touch ~/.emacs-custom.el
	@./tangle.sh emacs.org

utils:
	@./tangle.sh utils.org

suckless:
	@./tangle.sh suckless.org
