#!/bin/sh

# Sync mirrors to check for updates
sudo pacman -Sy >/dev/null

packagesCount="$(paru -Qu | wc -l)"

if [[ packagesCount -gt 0 ]]; then
  printf " $packagesCount"
fi
