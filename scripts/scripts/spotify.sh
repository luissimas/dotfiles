#!/bin/zsh

killall -q spotifyd
killall -q spt

spotifyd

kitty --class spotify-tui -e spt
