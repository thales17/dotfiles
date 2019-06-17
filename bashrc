#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

export PATH=$PATH:$HOME/.local/bin
export LANG='en_US.UTF-8'

export PATH=$PATH:$HOME/.cargo/bin

GREEN="\[$(tput setaf 2)\]"
RED="\[$(tput setaf 1)\]"
YELLOW="\[$(tput setaf 3)\]"
BLUE="\[$(tput setaf 4)\]"
PURPLE="\[$(tput setaf 5)\]"
CYAN="\[$(tput setaf 6)\]"
COLOR=$GREEN
BOLD="\[$(tput bold)\]"
RESET="\[$(tput sgr0)\]"
PROMPT='[\u@\h \W]'
PS1="${COLOR}[\u@${BOLD}${YELLOW}\h${RESET}${COLOR} \W]${PURPLE}\$${RESET} "


alias suhi='sudo systemctl suspend-then-hibernate'
