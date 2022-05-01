#!/bin/bash

percentage="$(free -m | grep Mem | awk '{printf "%d", (100*$3)/$2}')"
memoryString="$(free -h | grep Mem | awk '{printf $3"/"$2}' | tr -t [i] [b])"

printf "  $percentage%%"
