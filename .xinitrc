#!/bin/zsh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

export XDG_CURRENT_DESKTOP=xmonad
export XDG_DESKTOP_DIR="$HOME/"
export XDG_DOWNLOAD_DIR="$HOME/"

if [[ $HOSTNAME == mag ]]
then
    xmodmap -e 'keycode 191 = Insert' #F13
    numlockx
    redshift -t 5500:3000 -b 1.0:0.5 -l 59:17 &
    xinput set-prop "Kingsis Peripherals ZOWIE Gaming mouse" "libinput Accel Profile Enabled" 0, 1
    xset dpms 1800 1800 1800
    xset s 1800
fi
if [[ $HOSTNAME == arc ]]
then
    xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" -0.25
fi

[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources


xset b off
urxvtd -q -f -o
xss-lock -- slock &

if [ -z "$WM" ]; then
  exec openbox-session
else
  exec $WM
fi
