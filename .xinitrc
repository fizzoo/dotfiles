#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

xset b off &
xset m 0 0 &
synclient AccelFactor=0 MinSpeed=2 MaxSpeed=2 &
pulseaudio --daemonize

XDG_CURRENT_DESKTOP=i3
export XDG_CURRENT_DESKTOP

exec i3
