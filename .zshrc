# The following lines were added by compinstall

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' completions 1
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|[._-]=* r:|=*'
zstyle ':completion:*' max-errors 3
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original true
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' prompt '%e errors'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' substitute 1
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/fizzo/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall


HISTFILE=~/.zhistory
HISTSIZE=1000
SAVEHIST=1000
setopt extendedglob histverify autopushd pushdsilent

bindkey -v
export KEYTIMEOUT=1
bindkey "^?" backward-delete-char
bindkey "^[[3~" delete-char

eval $(dircolors /etc/fizz/.dircolors)
autoload -U colors && colors
autoload -U promptinit && promptinit
export PROMPT='%K{magenta}%(?..[%?])%1(j.{%j}.) %n %3~ %k'

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

#synca klockan
alias synctime='sudo systemctl restart chrony.service && sleep 10 && chronyc -a makestep && sleep 1 && timedatectl'

#kör reflector och mata in den i mirrorlist för bra mirrors
alias mirrorup='cp -vf /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.backup && reflector --verbose -l 100 -n 25 --sort rate --save /etc/pacman.d/mirrorlist'

alias syn='rsync --size-only --del -vrun '

alias g='g++ -std=c++11 -g '

alias ls='ls --color=always'
alias l='ls -A'
alias ll='ls -Al'

source /etc/fizz/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
