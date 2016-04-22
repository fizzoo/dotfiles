export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='google-chrome-stable'

export PATH=/home/fizzo/.cabal/bin:$PATH

if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

if [[ ! -d "$TMPDIR" ]]; then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"

if [[ -z $DISPLAY && $(tty) = /dev/tty1 && ! -a /tmp/x_started_dummy ]]; then
  touch /tmp/x_started_dummy
  exec startx
fi
