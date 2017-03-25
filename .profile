export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='firefox'

export _JAVA_AWT_WM_NONREPARENTING=1

[[ $SHELL=/bin/bash ]] && source ~/.bashrc

if [[ -z $DISPLAY && $(tty) = /dev/tty1 && ! -a /tmp/x_started_dummy ]]; then
  touch /tmp/x_started_dummy
  exec startx
fi
