#SHELL
alias ls='ls -GlhF'
if [ "$(uname)" == "Linux" ]; then
  alias ls='ls --color=auto -GlhF'
fi

export PS1='[\u@\h \W]\$ '

#SSH
addKey() { cat $HOME/.ssh/id_rsa.pub | ssh "$@" "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"; }

#FG
alias stela='stela -ca=$HOME/certs/server/ca.crt -cert=$HOME/certs/server/server.crt -key=$HOME/certs/server/server.key -port=31003 -multicast=31055 | humanlog --light-bg'
alias timeline='timeline -ca=$HOME/certs/client/ca.crt -cert=$HOME/certs/client/client.crt -key=$HOME/certs/client/client.key -stela=127.0.0.1:31003 -id=0 -config=$pscripts/peabody.toml -leader=true|humanlog --light-bg'
