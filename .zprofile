export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='chromium'

if [[ -z "$LANG" ]]; then
    export LANG='en_US.UTF-8'
fi

if [[ ! -d "$TMPDIR" ]]; then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi

TMPPREFIX="${TMPDIR%/}/zsh"

if [[ -z $startxstarted && -z $DISPLAY && $XDG_VTNR -eq 1 && -z $TMUX ]]
then
    startxstarted='y'
    printf "Starting X with gnome. Press anything to cancel.\n"
    read -k -t 2 || startx
fi
