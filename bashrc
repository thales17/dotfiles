#SHELL
alias ls='ls -GlhF'
export PS1='[\u@\h \W]\$ '

#SSH
addKey() { cat $HOME/.ssh/id_rsa.pub | ssh "$@" "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys"; }
