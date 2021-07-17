#!/bin/sh

selected=$(echo -e "F4Y\nIntroduction to Functional Programming" | rofi -dmenu -i -p "Class: ")

case $selected in "F4Y")
  brave https://meet.google.com/oij-tpte-ufw?authuser=2
  ;;
"Introduction to Functional Programming")
  brave https://meet.google.com/vcz-tcrs-stb?authuser=0
  ;;
esac
