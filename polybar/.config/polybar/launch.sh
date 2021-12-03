#!/bin/bash

# Terminate already running bar instances
killall -q polybar

polybar -rq bar >> /tmp/polybar.log & disown

echo "Polybar launched..."
