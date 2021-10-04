all: config emacs utils suckless

config:
	@touch ~/.bash_env
	@touch ~/.gitconfig-user
	@./tangle.sh config.org

emacs:
	@touch ~/.emacs-custom.el
	@./tangle.sh emacs.org

utils:
	@./tangle.sh utils.org

suckless:
	@./tangle.sh suckless.org
