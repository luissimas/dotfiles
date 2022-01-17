#!/bin/sh

selected=$(echo -e "F4Y\nCDN\nAlgLin\nEng. Software" | dmenu -i -p "Meet: ")

case $selected in
"F4Y")
	brave https://meet.google.com/zpo-nwqn-vrr?authuser=2
	;;
"CDN")
	brave https://meet.google.com/nxm-htfg-yzn?authuser=2
	;;
"Eng. Software")
	brave https://meet.google.com/nct-gwmx-bke?authuser=2
	;;
"AlgLin")
	brave http://meet.google.com/pfz-ahza-vnc
	;;
esac
