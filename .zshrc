
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

# Prompt is overridden by liquidprompt, if available
export PROMPT='%B%(!,%F{red},%F{green})%(0?,>,!)> %f%b'

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

c () {
  dir=$(find "$@" -xdev -print 2> /dev/null | fzf)
  if [[ -z "$dir" ]]; then return; fi
  if [[ ! -d "$dir" ]]; then dir=$(dirname "$dir"); fi
  cd "$dir" || exit 1
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

alias l='pwd;ls --color=always -lh'
alias ll='pwd;ls --color=always -Alh'
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

test -f $HOME/.dircolors && eval $( dircolors -b $HOME/.dircolors )
test -f $HOME/.zshtmp && . $HOME/.zshtmp
