#!/bin/sh

selected=$(echo -e "F4Y\nCN\nAlgLin\nEng. Software\nORI" | dmenu -i -p "Meet: ")

case $selected in
"F4Y")
	brave https://meet.google.com/zpo-nwqn-vrr?authuser=2
	;;
"CN")
	brave https://meet.google.com/nxm-htfg-yzn?authuser=2
	;;
"Eng. Software")
	brave https://meet.google.com/nct-gwmx-bke?authuser=2
	;;
"AlgLin")
	brave http://meet.google.com/pfz-ahza-vnc?authuser=2
	;;
"ORI")
	brave https://meet.google.com/pkk-bbyf-ahy?authuser=2
	;;
esac
