#!/bin/bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use
# polybar-msg cmd quit
# Otherwise you can use the nuclear option:
killall -q polybar

# Launch main bar
echo "---" | tee -a /tmp/polybar.log
polybar main 2>&1 | tee -a /tmp/polybar_main.log &
disown

# Launch second monitor bar (if available)
if [[ $(xrandr -q | grep 'HDMI-0 connected') ]]; then
	echo "---" | tee -a /tmp/polybar.log
	polybar secondary 2>&1 | tee -a /tmp/polybar_seconday.log &
	disown
fi

echo "Bars launched..."
