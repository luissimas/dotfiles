#!/bin/zsh

source ~/.cache/wal/colors.sh

bspc config normal_border_color "$background"
bspc config active_border_color "$foreground"
bspc config focused_border_color "$foreground"
bspc config presel_feedback_color "$color8"
