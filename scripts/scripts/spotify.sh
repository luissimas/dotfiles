#!/bin/zsh

killall -q spotifyd
killall -q spt

spotifyd

alacritty --class Alacritty,spotify-tui -e spt
