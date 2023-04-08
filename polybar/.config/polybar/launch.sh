#!/bin/bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use
# polybar-msg cmd quit
# Otherwise you can use the nuclear option:
killall -q polybar


# Dual monitor setup
if [[ $(xrandr -q | grep 'HDMI2 connected') ]]; then
  # Launch main bar
  echo "---" | tee -a /tmp/polybar.log
  polybar main 2>&1 | tee -a /tmp/polybar.log &
  disown

  echo "---" | tee -a /tmp/polybar.log
  polybar secondary 2>&1 | tee -a /tmp/polybar_seconday.log &
  disown
else
  # Launch main bar
  echo "---" | tee -a /tmp/polybar.log
  polybar main 2>&1 | tee -a /tmp/polybar.log &
  disown
fi

echo "Bars launched..."
