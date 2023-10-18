#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

xrdb -merge ~/.Xresources &
setxkbmap -layout "br,us" -variant "thinkpad,intl" -model "abnt2," -option "caps:swapescape" &

# Setting mouse sensitivity
xinput --set-prop 10 'libinput Accel Speed' 1

if [ $(xrandr -q | grep 'HDMI2 connected') ]; then autorandr desk; fi

unclutter --timeout 1 &
~/.fehbg &
wal -R &
picom &
# mopidy &
dunst &
batsignal -c 10 -w 15 -f 99 -e &
emacs --daemon --with-profile doom &

# Redshift controlling backlight temperature and color
# redshift -l $(curl -s "https://location.services.mozilla.com/v1/geolocate?key=geoclue" | awk 'OFS=":" {print $3,$5}' | tr -d ',}') &

# Starting polkit agent
polkit-dumb-agent &

exec bspwm
