zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' '+r:|[._-]=* r:|=*' '+l:|=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt '%e errors'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
autoload -Uz compinit && compinit

HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob histverify autopushd pushdsilent nobeep hist_ignore_all_dups

REPORTTIME=1

bindkey -v
export KEYTIMEOUT=1
bindkey "^?" backward-delete-char
bindkey "^[[3~" delete-char
bindkey -M vicmd '?' history-incremental-search-backward

eval $(dircolors /etc/fizz/.dircolors)
export PROMPT='%K{magenta}%(?..[%?])%1(j.{%j}.) %n %3~ %k'

umask 002

# functions & aliases
color(){
    for i in {0..256}
    do
        tput setab $i
        printf %$(tput cols)s
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

# just kill mpd after ncmpcpp closes, i have enough space to let it have a window...
musik(){
    mpd
    ncmpcpp
    mpd --kill
}

twi(){
    livestreamer twitch.tv/$1 best
}

#execute reflector and put the result in the mirrorlist
alias mirrorup='cp -vf /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup && reflector --verbose -l 64 -n 16 --sort rate --save /etc/pacman.d/mirrorlist'

alias syn='rsync --size-only --del -vrun '

alias g='g++ -std=c++11 -g '

alias l='ls --color=always -A'
alias ll='ls --color=always -al'
alias d='du -d1 -h'

alias ats='tmux -S /tmp/1'
alias at='tmux -S /tmp/1 attach'
alias wlo='sudo iftop -i wlo1'

alias gs='git status'
alias ga='git add'
alias gc='git commit'

if [[ $HOST == mag ]]
then
    rut(){
        sudo systemctl start nginx
        sudo systemctl start php-fpm
        printf 'http://localhost/rutorrent'
    }
fi
