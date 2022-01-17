#!/bin/sh

selected=$(man -k . | awk '{print $1}' | dmenu -i -p "Select a man")

if [[ -n "$selected" ]]; then
	man -Tpdf $selected | zathura -
fi
