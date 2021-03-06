#!/bin/sh

ethStatus="$(cat /sys/class/net/enp*/operstate 2>/dev/null)"
wifiStatus="$(cat /sys/class/net/w*/operstate 2>/dev/null)"
wifiName="$(iwgetid -r)"

if [[ $ethStatus = "up" ]]; then
	printf ""
elif [[ $wifiStatus = "up" ]]; then
  printf  "  $wifiName"
else
	printf " Disconnected"
fi

