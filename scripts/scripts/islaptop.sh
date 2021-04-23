#!/bin/sh

if [ -e "/sys/class/power_supply/BAT1" ]; then
	echo "É laptop"
else
	echo "É desktop"
fi
