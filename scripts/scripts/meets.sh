#!/bin/sh

selected=$(echo -e "F4Y" | rofi -dmenu -i -p "Class: ")

case $selected in
  "F4Y")
  brave https://meet.google.com/zpo-nwqn-vrr?authuser=2
  ;;
esac
