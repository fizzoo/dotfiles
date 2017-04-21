export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='firefox'
export _JAVA_AWT_WM_NONREPARENTING=1

[[ $SHELL = /bin/bash ]] && source ~/.bashrc
[[ $SHELL = /bin/zsh ]] && source ~/.zshrc

if [[ $(id -u) != 0 && -z $DISPLAY && $(tty) = /dev/tty1 && ! -a /tmp/x_started_dummy ]]; then
  touch /tmp/x_started_dummy
  exec startx
fi
