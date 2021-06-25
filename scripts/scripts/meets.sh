#!/bin/sh

selected=$(echo -e "Probabilidade\nAtendimento LM\nIntroduction to Functional Programming" | rofi -dmenu -i -p "Class: ")

case $selected in "Probabilidade")
  brave https://meet.google.com/dgv-bvnd-rjs?authuser=1
  ;;
"Atendimento LM")
  brave https://meet.google.com/yok-roat-gqg?authuser=1
  ;;
"Introduction to Functional Programming")
  brave https://meet.google.com/vcz-tcrs-stb?authuser=0
  ;;
esac
