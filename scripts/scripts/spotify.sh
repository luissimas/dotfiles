#!/bin/zsh

killall -q spotifyd

spotifyd

alacritty --class Alacritty,spotify-tui -e spt
