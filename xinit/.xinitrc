#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap


if [ -e "/sys/class/power_supply/BAT1" ]; then
  setxkbmap -model abnt2 -layout br -variant abnt2 &
  xrandr --newmode "1920x1080"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync && xrandr --addmode Virtual1 1920x1080 && xrandr --output Virtual1 --mode 1920x1080 &
else
  setxkbmap -layout us -variant padawan &
  xrandr --output HDMI-0 --left-of DVI-D-0 &
fi

unclutter --timeout 1 &
~/.fehbg &
wal -R &
picom &
mopidy &
emacs --daemon &

# Redshift controlling backlight temperature and color
redshift -b 1:0.5 -l -27.59667:-48.54917 &

# Starting gnome-keyring for mailspring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)&
export $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg)&
dbus-update-activation-environment --systemd DISPLAY&

exec bspwm
