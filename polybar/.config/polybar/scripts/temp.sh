#!/bin/bash

if [ $(find /sys/class/power_supply -name "*BAT*") ]; then
	# Laptop
	temp="$(sensors 2>/dev/null | grep 'Core 0' | awk 'NR==1{printf "%d", $3}')"
else
	# Desktop
	temp="$(sensors 2>/dev/null | grep Tctl | awk 'NR==1{printf "%d", $2}')"
fi

printf " $temp°C"
