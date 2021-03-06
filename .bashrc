export EDITOR='vim'
export VISUAL='vim'
export PAGER='less'
export BROWSER='firefox'
export _JAVA_AWT_WM_NONREPARENTING=1

export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=88888
export HISTFILESIZE=88888
shopt -s histappend

bind 'set show-all-if-ambiguous on'
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'


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
  if [ ! -d "$2" ]; then
    mkdir -p "$2"
  fi
  cp -r "$1" "$2"
}

c () {
  rootfind="."
  if [[ ! -z "$1" && -d "$1" ]]; then rootfind="$1"; fi
  dir=$(find "$rootfind" -xdev -print 2> /dev/null | fzf)
  if [[ -z "$dir" ]]; then return; fi
  if [[ ! -d "$dir" ]]; then dir=$(dirname "$dir"); fi
  cd "$dir"
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

alias l='ls --color=always -l'
alias ll='ls --color=always -Al'
alias d='du -had1 | sort -h'

ats () { sg a "tmux -S /tmp/1"; }
at () { sg a "tmux -S /tmp/1 attach"; }
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

[[ $- = *i* ]] && test -f /opt/liquidprompt/liquidprompt && source /opt/liquidprompt/liquidprompt
test -f $HOME/.dircolors && eval $( dircolors -b $HOME/.dircolors )
