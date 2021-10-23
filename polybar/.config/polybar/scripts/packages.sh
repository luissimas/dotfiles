#!/bin/sh

# Sync mirrors to check for updates
sudo pacman -Sy > /dev/null 2>&1

packagesCount="$(paru -Qu 2>/dev/null | wc -l)"

if [[ packagesCount -gt 0 ]]; then
  printf " $packagesCount"
fi
