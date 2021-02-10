#!/bin/zsh

wallpaperDirectory=/home/padawan/.wal/

# Prompt if the script should use the wallpaper or the themes
option=$(echo -e 'Wallpaper\nTheme\nPywal' | rofi -dmenu -p "Source")

case $option in "Pywal")
  selected=$(ls $wallpaperDirectory | rofi -dmenu -i -p "Select a file")

  if [[ -z "$selected" ]]; then
    printf "No image selected\n"
  else
    # Update pywal theme
    printf "Setting pywal theme...\n"
    wal -i "$wallpaperDirectory$selected" -q
    printf "Done.\n\n"

    # Update bspwm colors
    ~/scripts/bspwm/bspwmcolors.sh

    # Update betterlockscreen (takes a while)
    printf "Setting lock screen...\n"
    betterlockscreen -u "$wallpaperDirectory$selected" >/dev/null 2>&1
    printf "Done.\n"
  fi
  ;;
"Theme")
  selected=$(echo -e 'Gruvbox\nNord\nDracula' | rofi -dmenu -p "Source")

  case $selected in "Gruvbox") theme="base16-gruvbox-hard";;
  "Nord") theme="base16-nord";;
  "Dracula") theme="base16-dracula";;
  *) printf "No option selected."
  esac

    wal --theme $theme

    # Update bspwm colors
    ~/scripts/bspwm/bspwmcolors.sh
  ;;
"Wallpaper")
  selected=$(ls $wallpaperDirectory | rofi -dmenu -i -p "Select a file")

  if [[ -z "$selected" ]]; then
    printf "No image selected\n"
  else
    #Set wallpaper with feh
    printf "Setting wallpaper...\n"
    feh --bg-fill $wallpaperDirectory$selected
    betterlockscreen -u "$wallpaperDirectory$selected" >/dev/null 2>&1
    printf "Done.\n\n"
  fi
  ;;
*)
  printf "No option selected\n"
  ;;
esac
