#!/bin/zsh

# This script was copied from https://github.com/Bugswriter/myyt and was modified for my own needs

# Your api key (its better to make that a cat command)
YT_API_KEY=AIzaSyAZVpG9uqFgbgGyNR9y46WVpvyaQIbO4Ms

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

  # Url string for the api
  urlstring="https://www.googleapis.com/youtube/v3/search?part=snippet&q=${query}&type=video&maxResults=10&key=${YT_API_KEY}"

  # Get search results and pipe into rofi
  selectedUrl="https://$(curl -s $urlstring | jq -r '.items[] | "\(.snippet.channelTitle) => \(.snippet.title) => youtu.be/\(.id.videoId)"' | rofi -dmenu -i -p "Select a video " -l 10 -width 80 | awk '{print $NF}')"

  # Checks if any video was selected
  if [[ -n "$selectedUrl" && "$selectedUrl" != "https://" ]]; then
    mpv $selectedUrl
  fi
fi
