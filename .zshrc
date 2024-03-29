# If AUTO_TMUX is set (probably as part of ssh command sent to the remote desktop), exec tmux in a default session.
if [[ -n "$AUTO_TMUX" ]] && WHICH_FISH=$(whence -p fish); then
  echo "Starting tm(ux) through fish."
  export WHICH_FISH
  exec "$WHICH_FISH" -i -c "tm new -A -s main"
fi

# start fish shell if interactive, it's available, and safeguard not already defined.
# doing this at the start since the rest of the file affects interactive (zsh) mode only.
if [[ $- = *i* && -z $WHICH_FISH ]] && WHICH_FISH=$(whence -p fish); then
  export WHICH_FISH
  exec "$WHICH_FISH" -i
fi

#completers, _approximate tolerates 1 - max-errors faults
zstyle ':completion:*' completer _complete _approximate
zstyle ':completion:*' max-errors 4

#menu + color on cd and recommended group-name for that
zstyle ':completion:*' menu select=1
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' group-name ''

#show it all, tab again to see the start of it, less keystrokes
LISTMAX=800

#ignore case if nothing found, allow writing something in the middle of a word
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]} l:|=* r:|=*'

#load the completing
autoload -Uz compinit && compinit

HISTFILE=~/.zhistory
HISTSIZE=800
SAVEHIST=800
setopt extendedglob histverify autopushd pushdsilent nobeep hist_ignore_all_dups hist_ignore_space inc_append_history

export PROMPT="%F{blue}${RANGER_LEVEL:+(r${RANGER_LEVEL})}%B%(!,%F{red},%F{green})%(0?,>,!)> %f%b"

bindkey -v
export KEYTIMEOUT=1
bindkey "^?" backward-delete-char
bindkey "^w" backward-kill-word
bindkey "^r" history-incremental-search-backward
bindkey "^a" beginning-of-line
bindkey "^e" end-of-line
bindkey "^[[3~" delete-char
bindkey "^[[A" up-line-or-search && bindkey "^[[B" down-line-or-search
bindkey '^xa' _expand_alias
bindkey '^[*' _expand_alias


# Exports, path and stuff

# Make path an array-unique, so no duplicates
typeset -U PATH path

# Then pathmunge is trivial
pathmunge () {
    if (( $# == 0 )); then
        echo $path
        return
    elif (( $# == 1 )); then
        path=($1 $path)
        return
    elif (( $# == 2 )); then
        if [[ $2 == "after" ]]; then
          path=($path $1)
        else
          echo "Unknown \$2: $2"
        fi
    fi
}

export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export R_LIBS_USER='/opt/R-user/'

# `less` colors, mainly for `man`.
export LESS_TERMCAP_mb=$(tput bold; tput setaf 3) # blink, rarely used
export LESS_TERMCAP_md=$(tput bold; tput setaf 1) # h1, bold
export LESS_TERMCAP_me=$(tput sgr0) # end bold, blink, underline
export LESS_TERMCAP_so=$(tput bold; tput setaf 5; tput setab 0) # help text on bottom
export LESS_TERMCAP_se=$(tput sgr0) # end standout
export LESS_TERMCAP_us=$(tput bold; tput setaf 2) # h2, underline
export LESS_TERMCAP_ue=$(tput sgr0) # end underline
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

pathmunge "$HOME/.local/bin"
pathmunge "/opt/cuda/bin"
alias an='pathmunge "/opt/anaconda3/bin"'

ensure_n_args () {
  if (( $1 != $2 )); then
    printf "Function %s wants %d parameters, got %d\n" $funcstack[2] $1 $2
    return -1
  fi
}
alias nargs0='ensure_n_args 0 $# || return $?'
alias nargs1='ensure_n_args 1 $# || return $?'
alias nargs2='ensure_n_args 2 $# || return $?'
alias nargs3='ensure_n_args 3 $# || return $?'
alias nargs4='ensure_n_args 4 $# || return $?'
alias nargs5='ensure_n_args 5 $# || return $?'

# functions & aliases
color () {
    for i in {0..255}
    do
        tput setab $i
        printf "%8s" $i
    done
    tput op
    echo
}

csv () {
    column -s, -t $@ | less -\#8 -S
}

perm () {
    namei -mo $(readlink -f $*)
}

jsondiff () {
    diff -u --color <(jq -S . $1) <(jq -S . $2)
}

serve () {
    TRAPINT () { sudo nginx -s stop; return 42; }
    nginxfile='/tmp/nginx.conf'
    printf "user fizzo a; events { worker_connections 1024; } http { server { root \"$PWD\"; autoindex on; } }" >$nginxfile

    sudo nginx -c $nginxfile
    printf "Started server on directory '$PWD'\n"

    while true; do sleep 1; done
}

cpr () {
    if (( $# <= 1 )); then
        echo "Requires at least 2 arguments."
        return
    fi
    if [ ! -d "${@: -1}" ]; then
        mkdir -p "${@: -1}"
    fi
    cp -rv $*
}

manrg () {
    rg -z "$@" /usr/share/man/man{0,1,2,4,5,6,7,8,n}
}

c () {
    dir=$(find "$@" -xdev -print 2> /dev/null | fzf)
    if [[ -z "$dir" ]]; then return; fi
    if [[ ! -d "$dir" ]]; then dir=$(dirname "$dir"); fi
    cd "$dir" || return -1
}

alias ck='c /k/ && pwd'
alias cm='c /media/* && pwd'

f () {
    rootfind="."
    if [[ ! -z "$1" && -d "$1" ]]; then rootfind="$1"; fi
    find $rootfind -xdev -print 2> /dev/null | fzf -m
}

res () {
    stty sane iutf8
}
cl () {
    res
    clear
}

wifi () {
    line=$(nmcli d wifi | tac | fzf +s)
    [[ -z $line ]] && return
    ssid=$(echo $line | sed 's/^.  //' | sed 's/ \+Infra.*//')
    nmcli -a d wifi connect "$ssid"
}

checkhs () {
    cp $1 /tmp/lel.hs && \
        echo "return []\nrunchecks = \$quickCheckAll" >> /tmp/lel.hs && \
        echo runchecks | ghci -XTemplateHaskell /tmp/lel.hs
}

e () {
    emacs -nw $*
}
ew () {
    emacs $* &!
}

gs () {
    emacs -nw --eval "(progn (magit-status)(delete-other-windows))"
}

cmak () {
    mkdir build
    cd build
    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ..
    make
}

alias syn='rsync --size-only --del -vrun '
alias crash='coredumpctl info -1'

alias g='g++ -std=c++14 -g '

_l_help () {
    if (( $# == 0 )); then
        echo `pwd`
    elif (( $# == 1 )); then
        echo $(readlink -f $1)
    fi
}
l () {
    _l_help $*
    ls --color=always -lh $*
}
ll () {
    _l_help $*
    ls --color=always -Alh $*
}
alias d='du -had1 | sort -h'
alias s='ranger'

ats () { sg a "tmux -S /tmp/tmuxs"; }
at () {
    if [[ ! -S /tmp/tmuxs ]]; then
        echo "no socket yet, wait for host to create."
    else
        sg a "tmux -S /tmp/tmuxs attach"
    fi
}
own () { sudo chown -R $USER: $*; }

alias ana='make clean && scan-build -enable-checker alpha --view make'

nixi () { nix-env -qaP --description ".*$1.*"; }
haskelly () {
    nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ $* ])"
}
pythony () {
    nix-shell -E "with import <nixpkgs> { }; with python35Packages; runCommand \"dummy\" { buildInputs = [ $* ]; } \"\""
}
spam () {
    for i in {1..100}; do
        echo $*
    done
}
pyprofile () {
    python -m cProfile -s cumtime $*
}

clean () {
    paccache -rvk 1
    paccache -urvk 0
    rm -vrf ~/.local/share/Trash
}

gitclean () {
    git clean -xdn
    echo -n "Proceed? (y)"
    read YESNO
    if [[ $YESNO =~ "^[Yy]$" ]]; then
        git clean -xdf
    fi
}

spectroview () {
  nargs1
  file=`mktemp --suffix .png`
  sox $1 -n remix 1 spectrogram -x 3000 -y 513 -z 120 -w Kaiser -o $file
  sxiv $file
  rm $file
}

test_and_src () {
    [[ -f $1 && -r $1 ]] && source $1
}

test -f $HOME/.dircolors && eval $( dircolors -b $HOME/.dircolors )
test_and_src $HOME/.zshtmp
test_and_src /usr/share/fzf/completion.zsh
test_and_src /usr/share/fzf/key-bindings.zsh

# In case tests fail, do not produce an error code on startup
true
