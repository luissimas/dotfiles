#!/bin/sh

selected=$(echo -e "F4Y\nBD\nCN\nAlgLin\nEng. Software\nORI\nIA" | dmenu -i -p "Meet: ")

case $selected in
"F4Y")
	brave https://meet.google.com/yjo-xzin-rxn?authuser=2
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
"IA")
	brave http://meet.google.com/rfo-ycku-aqo?authuser=1
	;;
"BD")
	brave https://meet.google.com/yaa-nczg-jwg?authuser=1
	;;

esac
