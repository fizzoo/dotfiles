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

HISTSIZE=1000
setopt extendedglob histverify autopushd pushdsilent nobeep hist_ignore_all_dups

bindkey -v
export KEYTIMEOUT=1
bindkey "^?" backward-delete-char
bindkey "^[[3~" delete-char
bindkey "^[[A" up-line-or-search && bindkey "^[[B" down-line-or-search
bindkey -M vicmd '?' history-incremental-search-backward

export PROMPT='%K{magenta}%(?..[%?])%1(j.{%j}.) %n %3~ %k'

umask 002

# functions & aliases
color(){
  for i in {0..256}
  do
    tput setab $i
    printf "%8s" $i
  done
  tput op
}

sensibleown(){
  sudo chown -R fizzo:a $*
  sudo chmod -R ug=rwX,o-rwx $*
}

serve(){
  TRAPINT(){ sudo nginx -s stop; return 42 }
  nginxfile='/tmp/nginx.conf'
  printf "user fizzo a; events { worker_connections 1024; } http { server { root \"$PWD\"; autoindex on; } }" >$nginxfile
  myip=$(ip a show wlo1 | ag "inet " | sed "s/inet //" | sed "s/\/.*$//" | sed "s/[ \t]*//")

  sudo nginx -c $nginxfile
  printf "Started server on directory '$PWD', localip '$myip'\n"

  while true; do; sleep 1; done
}

# just kill mpd after ncmpcpp closes, let it have a workspace
musik(){
  mpd
  ~/.config/albumart/albumart.py &>/tmp/aalog &
  ncmpcpp
  kill %~/.config/albumart
  mpd --kill
}

twi(){
  livestreamer twitch.tv/$1 best
}

c(){
  file=$(find / -print 2> /dev/null | fzf)
  if [[ -z $file ]]; then return; fi
  if [[ -d $file ]]
  then
    dir=$file
  else
    dir=$(dirname $file)
  fi
  cd $dir
}

alias syn='rsync --size-only --del -vrun '
alias crash='coredumpctl info -1'

alias g='g++ -std=c++14 -g '

alias l='ls --color=always -al'
alias d='du -had1'

alias ats='tmux -S /tmp/1'
alias at='tmux -S /tmp/1 attach'

alias gs='git status'
alias ga='git add'
alias gc='git commit'
