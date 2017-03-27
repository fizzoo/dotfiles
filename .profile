[[ $SHELL=/bin/bash ]] && source ~/.bashrc

if [[ -z $DISPLAY && $(tty) = /dev/tty1 && ! -a /tmp/x_started_dummy ]]; then
  touch /tmp/x_started_dummy
  exec startx
fi
