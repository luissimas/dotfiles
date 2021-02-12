#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

setxkbmap -layout us -variant intl &
#setxkbmap -model abnt2 -layout br -variant abnt2
#xrandr --newmode "1920x1080"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync && xrandr --addmode Virtual1 1920x1080 && xrandr --output Virtual1 --mode 1920x1080 &
xrandr --output HDMI-0 --left-of DVI-D-0
unclutter --timeout 1 &
~/.fehbg &
wal -R &
picom &
exec bspwm
