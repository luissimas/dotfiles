#!/bin/sh

# Laptop command
volume="$(amixer get Master | grep 'Mono:' | awk '{print $4}' | tr -d '[%]')"

if [ $volume -gt 70 ] || [ $volume -eq 70 ]; then
	icon=""
elif [ $volume -lt 70 ] && [ $volume -gt 0 ]; then
	icon="墳"
else
	icon=""
fi

printf "$icon $volume%%"
