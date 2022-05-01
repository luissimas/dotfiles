#!/bin/bash

if [ $(find /sys/class/power_supply -name "*BAT*") ]; then
	# Desktop
	temp="$(sensors 2>/dev/null | grep Tctl | awk 'NR==1{printf "%d", $2}')"
else
	# Laptop
	temp="$(sensors 2>/dev/null | grep temp1 | awk 'NR==1{printf "%d", $2}')"
fi

printf " $temp°C"
