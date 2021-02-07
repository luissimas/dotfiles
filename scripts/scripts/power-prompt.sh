#!/bin/zsh

selected=$(echo -e 'No\nYes' | rofi -dmenu -p "Shutdown?" -l 2)

if [ $selected = "Yes" ]; then
  exec shutdown now
fi
