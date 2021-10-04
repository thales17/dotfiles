all: config emacs utils suckless

config:
	@touch ~/.bash_env
	@touch ~/.emacs-custom.el
	@./tangle.sh config.org

emacs:
	@./tangle.sh emacs.org

utils:
	@./tangle.sh utils.org

suckless:
	@./tangle.sh suckless.org
