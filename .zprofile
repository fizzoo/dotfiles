export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='chromium'

export PATH=/home/fizzo/.cabal/bin:$PATH

if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

if [[ ! -d "$TMPDIR" ]]; then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
