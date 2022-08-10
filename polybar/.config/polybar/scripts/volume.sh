#!/bin/bash

# Laptop command
status="$(amixer get Master | grep 'Front Left:' | awk '{print $6}' | tr -d '[%]')"
volume="$(amixer get Master | grep 'Front Left:' | awk '{print $5}' | tr -d '[%]')"

if [[ $status = "off" ]]; then
	printf "ﱝ mute"
else
	if [[ $volume -gt 70 ]] || [[ $volume -eq 70 ]]; then
		icon=" "
	elif [[ $volume -lt 70 ]] && [[ $volume -gt 0 ]]; then
		icon="墳 "
	else
		icon=" "
	fi

	printf "$icon $volume%%"
fi
