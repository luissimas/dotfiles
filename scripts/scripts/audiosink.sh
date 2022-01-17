#!/bin/sh

selected=$(pamixer --list-sinks | grep alsa | dmenu -i -p "Select output device" | awk '{print $1}')

if [ -z "$selected" ]; then
	echo "No device selected"
else
	pacmd set-default-sink $selected
fi
