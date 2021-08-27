#!/bin/sh

selected=$(echo -e "SO\nMD\nFIL\nIFP\nF4Y" | rofi -dmenu -i -p "Class: ")

case $selected in
  "SO")
  brave https://meet.google.com/tzx-ojdd-xqk?authuser=1
  ;;
  "MD")
  brave meet.google.com/vzr-qkpu-iec?authuser=1
  ;;
  "FIL")
  brave https://meet.google.com/xpt-ybdj-vrt?authuser=1
  ;;
  "IFP")
  brave https://meet.google.com/vcz-tcrs-stb?authuser=0
  ;;
  "F4Y")
  brave https://meet.google.com/zpo-nwqn-vrr?authuser=2
  ;;
esac
