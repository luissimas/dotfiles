#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

setxkbmap -model abnt2 -layout br -variant thinkpad -option "caps:swapescape" &

unclutter --timeout 1 &
~/.fehbg &
wal -R &
# picom &
# mopidy &
dunst &
batsignal -c 10 -w 30 -f 99
emacs --daemon --with-profile doom &

# Redshift controlling backlight temperature and color
# redshift -b 1:0.5 -l -27.59667:-48.54917 &

# Starting polkit agent
polkit-dumb-agent &

exec bspwm
