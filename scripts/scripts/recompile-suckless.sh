#!/bin/sh

dwmDir=/home/padawan/repos/dwm
stDir=/home/padawan/repos/st

compileDwm() {
	cd $dwmDir
	make clean install >/dev/null
}

compileSt() {
	cd $stDir
	make clean install >/dev/null
}

#printf "Recompiling dwm...\n"
#compileDwm
#printf "Done.\n\n"

printf "Recompiling st...\n"
compileSt
printf "Done.\n"
