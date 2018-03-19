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

#node
export NPM_GLOBAL=$HOME/.npm-global
export PATH=$PATH:$NPM_GLOBAL/bin

#PROJECT DIR
export stela=$GOPATH/src/github.com/forestgiant/stela
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
export pui=$GOPATH/src/gitlab.fg/peabody/ui
export nodevideo=$HOME/code/node-video
export cage=$GOPATH/src/gitlab.fg/flyingaxes/cage
export scoreboard=$GOPATH/src/gitlab.fg/flyingaxes/scoreboard
export coach=$GOPATH/src/gitlab.fg/flyingaxes/coach
export fascripts=$GOPATH/src/gitlab.fg/flyingaxes/scripts
export dotfiles=$HOME/code/dotfiles
export wifi=$GOPATH/src/gitlab.fg/go/wifi
export scheduleit=$HOME/code/scheduleit
export oauthgrpc=$HOME/code/go/src/github.com/forestgiant/oauthgrpc
export harmony=$HOME/code/go/src/gitlab.fg/otis/harmony
export protodapui=$HOME/code/scratch/protodap-ui
export echonat=$GOPATH/src/gitlab.fg/go/echonat
export storywise=$HOME/code/xcode/storywise
export rtproto=$HOME/code/realtree/prototype
export rtchooser=$HOME/code/realtree/rt-chooser
export xlsx2json=$GOPATH/src/gitlab.fg/realtree/xlsx2json
export disco=$GOPATH/src/github.com/forestgiant/disco
export goeuler=$GOPATH/src/github.com/thales17/go-projecteuler
export forestgiantdotcom=$HOME/code/forestgiant/forestgiant.com
export edgemicrosite=$HOME/code/realtree/edge-microsite
export canvastetris=$HOME/code/canvas-tetris
export lge=$HOME/code/go/src/gitlab.fg/lge
export servicetracker=$HOME/code/go/src/gitlab.fg/lge/service-status-tracker
#MINIO
export ACCESS_KEY=3RDSZGTI0740DBZY47SE
export SECRET_KEY=NIRhy65ROMStmMyfm55wihLEWy4eRVxsBs4MiPib

if [ "$(uname)" == "Linux" ]; then
  dropbox &
  xscreensaver -no-splash &
fi

export downloads=$HOME/Downloads
export code=$HOME/code
