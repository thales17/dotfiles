#ENABLE CLI COLOR
export CLICOLOR=1
#BSD LS COLORS
export LSCOLORS=exfxcxdxbxegedabagacad

#EDITOR
export EDITOR=vim
#PATH
export PATH=$PATH:$HOME/bin

#GO
export GOPATH=$HOME/code/go
export PATH=$PATH:$GOPATH/bin

#PROJECT DIR
export stela=$GOPATH/src/gitlab.fg/go/stela
export iris=$GOPATH/src/gitlab.fg/otis/iris
export slideshow=$GOPATH/src/gitlab.fg/otis/slideshow
export magbooth=$HOME/code/magbooth/magbooth-2
export eff=$GOPATH/src/github.com/forestgiant/eff
export goscratch=$GOPATH/src/gitlab.fg/scratch 
export otisapp=$GOPATH/src/gitlab.fg/otis/otisapp
export otisdocs=$HOME/code/otis/docs/
export stelanode=$HOME/code/otis/stela-nodejs
export pedestal=$GOPATH/src/gitlab.fg/magicforest/pedestal
export pedestalhw=$HOME/code/magicforest/pedestal-hardware
export timeline=$GOPATH/src/gitlab.fg/peabody/timeline
export dmxlight=$GOPATH/src/gitlab.fg/peabody/dmxlight
export pscripts=$GOPATH/src/gitlab.fg/peabody/script
export humanlog=$GOPATH/src/github.com/aybabtme/humanlog
export arduinolight=$GOPATH/src/gitlab.fg/peabody/arduinolight

#MINIO
export ACCESS_KEY=3RDSZGTI0740DBZY47SE
export SECRET_KEY=NIRhy65ROMStmMyfm55wihLEWy4eRVxsBs4MiPib

if [ "$(uname)" == "Linux" ]; then
  dropbox &
fi
