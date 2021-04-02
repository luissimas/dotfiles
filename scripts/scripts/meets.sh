#!/bin/sh

selected=$(echo -e "Probabilidade\nAtendimento Arq\nAtendimento POO" | rofi -dmenu -i -p "Class: ")

case $selected in "Probabilidade")
  brave https://meet.google.com/faj-syow-bpk
  ;;
"Atendimento POO")
  brave https://meet.google.com/sei-qyvm-uhs
  ;;
"Atendimento Arq")
  brave https://meet.google.com/dfh-aotz-pfc
  ;;
esac
