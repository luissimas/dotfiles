#!/bin/sh

# Import the colors
. "${HOME}/.cache/wal/colors.sh"

dmenu_run -nb "$background" -nf "$foreground" -sb "$foreground" -sf "$background"
