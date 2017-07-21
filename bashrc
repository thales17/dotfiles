#SHELL
alias ls='ls -GlhF'
if [ "$(uname)" == "Linux" ]; then
  alias ls='ls --color=auto -GlhF'
fi

export PS1='[\u@\h \W]\$ '

#TODOTXT
alias t='$HOME/bin/todo.txt_cli-2.10/todo.sh -d $HOME/bin/todo.txt_cli-2.10/todo.cfg'

#SSH
addKey() { cat $HOME/.ssh/id_rsa.pub | ssh "$@" "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"; }

#PROTOBUFS
protogo() { protoc --proto_path=./ --go_out=plugins=grpc:./ ./"$1"; }



