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

export GOPROXY=direct
export GOSUMDB=off
export GOTELEMETRY=off
export GOPATH=$HOME/go
export GOROOT=/usr/local/go
export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

export DOTNET_CLI_TELEMETRY_OPTOUT=1

source $HOME/.bash_env

export EDITOR=mg

alias e='emacsclient -t'
