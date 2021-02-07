#!/bin/sh

temp="$(sensors | grep temp1 | awk 'NR==1{printf "%d", $2}')"

printf "﬙ $temp°C"
