#!/bin/zsh


selected=$(pamixer --list-sinks | grep alsa | rofi -dmenu -i -width 60 -p "Select output device" | awk '{print $1}')

if [ -z "$selected" ]; then
  echo "No device selected"
else
  pacmd set-default-sink $selected
fi

