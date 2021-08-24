#!/bin/sh

selected=$(echo -e "SO\nMD\nF4Y\nIFP" | rofi -dmenu -i -p "Class: ")

case $selected in
  "SO")
  brave https://meet.google.com/tzx-ojdd-xqk?authuser=1
  ;;
  "MD")
  brave meet.google.com/vzr-qkpu-iec?authuser=1
  ;;
  "F4Y")
  brave https://meet.google.com/zpo-nwqn-vrr?authuser=2
  ;;
  "IFP")
  brave https://meet.google.com/vcz-tcrs-stb?authuser=0
  ;;
esac
