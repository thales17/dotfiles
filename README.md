# Dot Files
This is a collection of dotfiles that I use across all of the computers I work on.

# Emacs
Inside the emacs folder you will find init.el and the elisp folder, both of these need to be symlinked into the `$HOME/.emacs.d` folder

## Elisp Folder
This folder has all custom emacs scripts, the init.el should have a line that adds this to the load path

## MELPA Packages
lua-mode
helm
helm-ag (*requires the silver searcher)
dumb-jump
go-mode
markdown-mode
magit
org
runner

# Linux
Various config files that are linux specific

## Xresources
This currently configures urxvt.  It requires the source-code-pro font to be installed.
Use `sudo pacman -S adobe-source-code-pro-fonts` to install in arch linux.
