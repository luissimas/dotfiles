#!/bin/zsh
filePath="/home/padawan/screenshots/"

# If no arguments are given, a rofi prompt is shown
if [[ -z "$1" ]]; then
  fileName=$(rofi -dmenu -p "File name " -l 0)
else
  fileName="$1"
fi

# Kill unclutter to allow mouse selection
killall unclutter

# Launch scrot
scrot -s $filePath$fileName.png

# Relaunch unclutter
unclutter&
