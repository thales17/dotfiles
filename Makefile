all: git


.PHONY: git
git:
	@touch $HOME/.gitconfig-user
	@stow -t $HOME -R git
