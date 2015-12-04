export GOPATH=$HOME/golang
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin
export GO15VENDOREXPERIMENT=1

# gv: Used to wrap go get to add -v flag 
export GOCOMMANDLOCATION=/usr/local/bin
source $GOPATH/src/github.com/forestgiant/gv/go_to_gv

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced
