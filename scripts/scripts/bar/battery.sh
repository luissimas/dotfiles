#!/bin/sh

if [ -e "/sys/class/power_supply/BAT1" ]; then
  batteryStatus="$(cat /sys/class/power_supply/BAT*/status)"
  capacity="$(cat /sys/class/power_supply/BAT*/capacity)"

  if [ $batteryStatus = "Charging" ]; then
    icon=""
  elif [ $capacity -ge 75 ]; then
    icon=" "
  elif [ $capacity -lt 75 ] && [ $capacity -ge 50 ]; then
    icon=" "
  elif [ $capacity -lt 50 ] && [ $capacity -ge 25 ]; then
    icon=" "
  elif [ $capacity -lt 25 ] && [ $capacity -ge 0 ]; then
    icon=" "
  fi

  printf "$icon$capacity%%"
fi

