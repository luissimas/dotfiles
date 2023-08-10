#!/bin/bash

option=$(echo -e 'Lock\nShutdown\nReboot\nLogout' | rofi -dmenu -i -p "Power prompt")

case $option in "Shutdown")
    exec shutdown now
    ;;
"Reboot")
    exec reboot
    ;;
"Logout")
    exec bspc quit
    ;;
"Lock")
    exec i3lock -B 10 -k
    ;;
esac
