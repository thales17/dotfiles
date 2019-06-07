#
# ~/.zrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

autoload -Uz compinit
compinit

PROMPT='%F{green}[%n%f@%F{yellow}%B%m%b%f %F{green}%~]%f%F{magenta}$%f '
alias ls='ls --color=auto'

export PATH=$PATH:$HOME/.local/bin
export LANG='en_US.UTF-8'

export PATH=$PATH:$HOME/.cargo/bin

alias suhi='sudo systemctl suspend-then-hibernate'
