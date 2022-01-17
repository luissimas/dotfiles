#!/bin/sh

option=$(echo -e 'Shutdown\nReboot' | dmenu -i -p "Power prompt")

case $option in "Shutdown")
	exec shutdown now
	;;
"Reboot")
	exec reboot
	;;
esac
