#!/bin/zsh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export XDG_CURRENT_DESKTOP=xmonad

if [[ $HOSTNAME == mag ]]
then
  xmodmap -e 'keycode 191 = Insert' #F13
  numlockx
fi
if [[ $HOSTNAME == arc ]]
then
  synclient HorizTwoFingerScroll=1
fi


xset b off
xset m 0 0
sh ~/.fehbg

stalonetray &
exec xmonad
