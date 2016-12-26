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
  redshift -t 5500:3000 -b 1.0:0.5 -l 59:17 &
  qbittorrent &
fi
if [[ $HOSTNAME == arc ]]
then
  synclient HorizTwoFingerScroll=1
  nm-applet &
fi

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

xset b off
xset m 0 0
sh ~/.fehbg
dunst &

exec xmonad
