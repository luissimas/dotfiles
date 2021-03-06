#!/bin/zsh

# This script was copied from https://github.com/Bugswriter/myyt and was modified for my own needs

# If no arguments are given, a rofi prompt is shown
if [[ -z "$1" ]]; then
  query=$(rofi -dmenu -p "Search " -l 0)
else
	query="$1"
fi

# Checks if the query is empty
if [[ -n "$query" && "$query" != " " ]]; then
  # Changing spaces to '+'
  query="${query// /+}"

  # Get search results and pipe into rofi using python web scraper
  selectedUrl=$(python ~/scripts/youtube-scraper.py $query | rofi -dmenu -i -p "Select a video " -l 10 -width 60 | awk '{print $NF}')

  # Checks if any video was selected
  if [[ -n "$selectedUrl" && "$selectedUrl" != "https://" ]]; then
    mpv $selectedUrl
  fi
fi
