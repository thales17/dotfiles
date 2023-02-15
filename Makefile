all: git tmux nethack abcde beets bash dunst bin emacs xmodmap alacritty gnupg sway

clean: clean-git
clean: clean-tmux
clean: clean-nethack
clean: clean-abcde
clean: clean-beets
clean: clean-bash
clean: clean-dunst
clean: clean-bin
clean: clean-emacs
clean: clean-xmodmap
clean: clean-alacritty
clean: clean-gnupg
clean: clean-sway

.PHONY: git
git:
	@echo "Git"
	@echo "##############################"
	@touch ${HOME}/.gitconfig-user
	@stow -t ${HOME} -R git

clean-git:
	@rm -fv ${HOME}/.gitconfig


.PHONY: tmux
tmux:
	@echo "Tmux"
	@echo "##############################"
	@stow -t ${HOME} -R tmux

clean-tmux:
	@rm -fv ${HOME}/.tmux.conf

.PHONY: nethack
nethack:
	@echo "Nethack"
	@echo "##############################"
	@stow -t ${HOME} -R nethack

clean-nethack:
	@rm -fv ${HOME}/.nethackrc

.PHONY: abcde
abcde:
	@echo "abcde"
	@echo "##############################"
	@stow -t ${HOME} -R abcde

clean-abcde:
	@rm -fv ${HOME}/.abcde.conf

.PHONY: beets
beets:
	@echo "beets"
	@echo "##############################"
	@mkdir -pv ${HOME}/.config/beets/data
	@stow -t ${HOME} -R beets

clean-beets:
	@rm -fv ${HOME}/.config/beets/config.yaml

.PHONY: bash
bash:
	@echo "bash"
	@echo "##############################"
	@stow -t ${HOME} -R bash

clean-bash:
	@rm -fv ${HOME}/.bash_env.example
	@rm -fv ${HOME}/.bashrc
	@rm -fv ${HOME}/.profile
	@rm -fv ${HOME}/.xsessionrc

.PHONY: dunst
dunst:
	@echo "dunst"
	@echo "##############################"
	@stow -t ${HOME} -R dunst

clean-dunst:
	@rm -Rfv ${HOME}/.config/dunst

.PHONY: bin
bin:
	@echo "bin"
	@echo "##############################"
	@mkdir -pv ${HOME}/.local/bin
	@stow -t ${HOME}/.local/bin -R bin

clean-bin:
	@rm -fv ${HOME}/.local/bin/volup
	@rm -fv ${HOME}/.local/bin/volmute
	@rm -fv ${HOME}/.local/bin/voldown
	@rm -fv ${HOME}/.local/bin/syncmusic
	@rm -fv ${HOME}/.local/bin/syncmail
	@rm -fv ${HOME}/.local/bin/sus
	@rm -fv ${HOME}/.local/bin/repo_backup
	@rm -fv ${HOME}/.local/bin/mp3_music
	@rm -fv ${HOME}/.local/bin/lightup
	@rm -fv ${HOME}/.local/bin/lightdown
	@rm -fv ${HOME}/.local/bin/ec
	@rm -fv ${HOME}/.local/bin/backup
	@rm -fv ${HOME}/.local/bin/capture_screen
	@rm -fv ${HOME}/.local/bin/capture_gif

.PHONY: emacs
emacs:
	@echo "emacs"
	@echo "##############################"
	@touch ${HOME}/.emacs-custom.el
	@mkdir -pv ${HOME}/.emacs.d
	@stow -t ${HOME} emacs

clean-emacs:
	@rm -fv ${HOME}/.emacs
	@rm -Rfv ${HOME}/.emacs.d/ajr.el
	@rm -fv ${HOME}/.emacs.d/ajr-1.el

.PHONY: xmodmap
xmodmap:
	@echo "xmodmap"
	@echo "##############################"
	@stow -t ${HOME} xmodmap

clean-xmodmap:
	@rm -fv ${HOME}/.Xmodmap

clean-alacritty:
	@rm -rfv ${HOME}/.config/alacritty

.PHONY: alacritty
alacritty:
	@echo "alacritty"
	@echo "##############################"
	@stow -t ${HOME} alacritty

clean-gnupg:
	@rm -rfv ${HOME}/.gnupg/gpg-agent.conf

.PHONY: gnupg
gnupg:
	@echo "gnupg"
	@echo "##############################"
	@mkdir -pv ${HOME}/.gnupg
	@chmod 700 ${HOME}/.gnupg
	@stow -t ${HOME} gnupg

clean-sway:
	@rm -rfv ${HOME}/.config/sway

.PHONY: sway
sway:
	@echo "sway"
	@echo "##############################"
	@stow -t ${HOME} sway
