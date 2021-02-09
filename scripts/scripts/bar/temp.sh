#!/bin/sh


isLaptop="$(cat /sys/class/power_supply/BAT*/status 2>/dev/null)"

if [ -z "$isLaptop" ]; then
  # Desktop
  temp="$(sensors | grep Tctl | awk 'NR==1{printf "%d", $2}')"
else
  # Laptop
  temp="$(sensors | grep temp1 | awk 'NR==1{printf "%d", $2}')"
fi

printf "﬙ $temp°C"
