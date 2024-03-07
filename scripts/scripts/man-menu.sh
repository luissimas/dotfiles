#!/usr/bin/env bash

selected=$(man -k . | awk '{print $1}' | rofi -dmenu -i -p "Select a man")

if [[ -n "$selected" ]]; then
    man -Tpdf $selected | zathura -
fi
