#!/bin/zsh

option=$(echo -e 'Shutdown\nReboot' | rofi -dmenu -p "Power prompt")

case $option in "Shutdown")
    exec shutdown now
    ;;
  "Reboot")
    exec reboot
    ;;
esac
