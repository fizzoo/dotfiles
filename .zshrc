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

if [[ $EUID = 0 ]]; then
  umask 022
  export PROMPT='%K{magenta}%(?..%K{cyan}[%?])%1(j.{%j}.) %n %3~ %k'
else
  umask 007
  export PROMPT='%K{green}%(?..%K{red}[%?])%1(j.{%j}.) %n %3~ %k'
fi

bindkey -v
export KEYTIMEOUT=1
bindkey "^?" backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[A" up-line-or-search && bindkey "^[[B" down-line-or-search
bindkey -M vicmd '?' history-incremental-search-backward

# functions & aliases
color(){
  for i in {0..256}
  do
    tput setab $i
    printf "%8s" $i
  done
  tput op
}

serve(){
  TRAPINT(){ sudo nginx -s stop; return 42 }
  nginxfile='/tmp/nginx.conf'
  printf "user fizzo a; events { worker_connections 1024; } http { server { root \"$PWD\"; autoindex on; } }" >$nginxfile

  sudo nginx -c $nginxfile
  printf "Started server on directory '$PWD'\n"

  while true; do; sleep 1; done
}

m(){
  if [[ ! -a /tmp/music_dummy ]]
  then
    mpd
    albumart 2>/dev/null & disown
    touch /tmp/music_dummy
  fi
  ncmpcpp
}

cpr(){
  if [ ! -d "$2" ]; then
    mkdir -p "$2"
  fi
  cp -r "$1" "$2"
}

twi(){
  streamlink twitch.tv/$1 best
}

c(){
  rootfind="."
  if [[ ! -z $1 && -d $1 ]]; then rootfind=$1; fi
  dir=$(find $rootfind -xdev -print 2> /dev/null | fzf)
  if [[ -z $dir ]]; then return; fi
  if [[ ! -d $dir ]]; then dir=$(dirname $dir); fi
  cd $dir
}

wifi(){
  line=$(nmcli d wifi | tac | fzf +s)
  [[ -z $line ]] && return
  ssid=$(echo $line | sed 's/^.  //' | sed 's/ \+Infra.*//')
  nmcli -a d wifi connect "$ssid"
}

checkhs(){
  cp $1 /tmp/lel.hs && \
  echo "return []\nrunchecks = \$quickCheckAll" >> /tmp/lel.hs && \
  echo runchecks | ghci -XTemplateHaskell /tmp/lel.hs
}

e(){
  emacs -nw $*
}
ew(){
  emacs $* &!
}

gs(){
  emacs -nw --eval "(progn (magit-status)(delete-other-windows))"
}

cmak(){
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
alias d='du -had1'

ats(){ sg a "tmux -S /tmp/1" }
at(){ sg a "tmux -S /tmp/1 attach" }

alias ana='make clean && scan-build -enable-checker alpha --view make'
