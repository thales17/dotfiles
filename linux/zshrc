#
# ~/.zrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

autoload -Uz compinit
compinit

PROMPT='%F{green}[%n%f@%F{yellow}%B%m%b%f %F{green}%~]%f%F{magenta}$%f '

export LANG='en_US.UTF-8'

export PATH=$PATH:$HOME/.local/bin
export PATH=$PATH:$HOME/.cargo/bin
export PATH=$PATH:$HOME/code/scripts

export SDL_VIDEODRIVER=wayland

alias ls='ls --color=auto'
alias suhi='sudo systemctl suspend-then-hibernate'
alias gitlog='git log --oneline --decorate --all --graph'
alias notez='grep --line-buffered --color=never -r "" ~/notes/* | fzf' 
alias icat='kitty +kitten icat'
