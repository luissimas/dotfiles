#!/bin/sh

selected=$(echo -e "Probabilidade\nAtendimento Arq\nAtendimento LM\nAtendimento AED" | rofi -dmenu -i -p "Class: ")

case $selected in "Probabilidade")
  brave https://meet.google.com/dgv-bvnd-rjs?authuser=1
  ;;
"Atendimento Arq")
  brave https://meet.google.com/dfh-aotz-pfc?authuser=1
  ;;
"Atendimento LM")
  brave https://meet.google.com/yok-roat-gqg?authuser=1
  ;;
"Atendimento AED")
  brave https://meet.google.com/stt-xswo-tcz?authuser=1
  ;;
esac
