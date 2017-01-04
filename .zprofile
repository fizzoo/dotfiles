export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='chrome'

export PATH=/home/fizzo/.cabal/bin:$PATH

if [[ -z $DISPLAY && $(tty) = /dev/tty1 && ! -a /tmp/x_started_dummy ]]; then
  touch /tmp/x_started_dummy
  exec startx
fi
