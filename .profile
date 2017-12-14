export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export R_LIBS_USER='/opt/R-user/'

path+="$HOME/.local/bin"
path+="/opt/cuda/bin/"
export path

[[ $SHELL = /bin/bash ]] && source ~/.bashrc
[[ $SHELL = /bin/zsh ]] && source ~/.zshrc

if [[ $(id -u) != 0 && -z $DISPLAY && $(tty) = /dev/tty1 && ! -a /tmp/x_started_dummy ]]; then
  touch /tmp/x_started_dummy
  exec startx
fi
