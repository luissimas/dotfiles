#!/usr/bin/env bash

selected=$(pamixer --list-sinks | grep alsa | rofi -dmenu -i -p "Select output device" | awk '{print $1}')

if [ -z "$selected" ]; then
    echo "No device selected"
else
    pacmd set-default-sink $selected
fi
