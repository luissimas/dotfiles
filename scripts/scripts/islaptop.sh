#!/bin/sh

if [ -d "/sys/class/power_supply/BAT*" ]; then
	echo "É laptop"
else
	echo "É desktop"
fi
