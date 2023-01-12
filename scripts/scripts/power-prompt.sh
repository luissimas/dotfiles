#!/bin/bash

option=$(echo -e 'Shutdown\nReboot\nLogout' | rofi -dmenu -i -p "Power prompt")

case $option in "Shutdown")
    exec shutdown now
    ;;
"Reboot")
    exec reboot
    ;;
"Logout")
    exec bspc quit
    ;;
esac
