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
    xset m 0 0
fi
if [[ $HOSTNAME == arc ]]
then
    # vert + horiz 2-finger scroll
    xinput --set-prop "SynPS/2 Synaptics TouchPad" "Synaptics Two-Finger Scrolling" 1 1
    # lower scale so deceleration is in effect more often
    xinput --set-prop "SynPS/2 Synaptics TouchPad" "Device Accel Velocity Scaling" 8
    # lower speed in general
    xinput --set-prop "SynPS/2 Synaptics TouchPad" "Device Accel Constant Deceleration" 4
fi

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources


xset b off
sh ~/.fehbg
dunst &

exec xmonad
