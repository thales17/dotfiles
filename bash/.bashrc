#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '

# Customization begins here
export LD_LIBRARY_PATH=$HOME/.local/lib

export PATH=$PATH:$HOME/.local/bin

export DOTNET_CLI_TELEMETRY_OPTOUT=1

source $HOME/.bash_env

export EDITOR=mg
