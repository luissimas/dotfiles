#!/bin/sh

selected=$(echo -e "F4Y\nCN\nAlgLin\nEng. Software\nORI" | dmenu -i -p "Meet: ")

case $selected in
"F4Y")
	brave https://meet.google.com/zpo-nwqn-vrr?authuser=2
	;;
"CN")
	brave https://meet.google.com/nxm-htfg-yzn?authuser=1
	;;
"Eng. Software")
	brave https://meet.google.com/nct-gwmx-bke?authuser=1
	;;
"AlgLin")
	brave http://meet.google.com/pfz-ahza-vnc?authuser=1
	;;
"ORI")
	brave https://meet.google.com/pkk-bbyf-ahy?authuser=1
	;;
esac
