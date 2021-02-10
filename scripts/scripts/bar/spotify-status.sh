#!/bin/sh

player="spotifyd"

formatString="{{ title }} - {{ artist }}"

playerctlStatus=$(playerctl --player=$player status 2>/dev/null)

# Only show data if there is a music playing at the moment
if [ $playerctlStatus = "Playing" ]; then
  musicData="$(playerctl --player=$player metadata --format "$formatString")"
  echo "  $musicData"
else
  echo ""
fi
