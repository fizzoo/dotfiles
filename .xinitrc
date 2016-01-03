#!/bin/zsh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xset b off &
xset m 0 0 &
synclient AccelFactor=0 MinSpeed=2 MaxSpeed=2 HorizTwoFingerScroll=1 &

XDG_CURRENT_DESKTOP=xmonad
export XDG_CURRENT_DESKTOP

if [[ $HOSTNAME == mag ]]
then
    xmodmap -e 'keycode 191 = Insert' #F13
    numlockx & #no numpad on arc
    variety &
fi

xcompmgr &
stalonetray &
sleep 1 && volumeicon &
exec xmonad | xmobar
