#!/bin/zsh

wallpaperDirectory=/home/padawan/.wal/

# Prompt if the script should use the wallpaper or the themes
option=$(echo -e 'Wallpaper\nTheme\nPywal' | rofi -dmenu -i -p "Source")

case $option in "Pywal")
  selected=$(ls $wallpaperDirectory | rofi -dmenu -i -p "Select a file")

  if [[ -z "$selected" ]]; then
    printf "No image selected\n"
  else
    themelight=$(echo -e 'Dark\nLight' | rofi -dmenu -i -p "Select")

    if [[ -z "$themelight" ]]; then
      printf "No selection\n"
    else
      # Update pywal theme
      printf "Setting pywal theme...\n"

      case $themelight in "Dark")
        wal -i "$wallpaperDirectory$selected" -q
      ;;
      "Light")
        wal -l -i "$wallpaperDirectory$selected" -q
      ;;
      esac

      printf "Done.\n\n"

      # Update bspwm colors
      ~/scripts/bspwm/bspwmcolors.sh

      # Update betterlockscreen (takes a while)
      printf "Setting lock screen...\n"
      betterlockscreen -u "$wallpaperDirectory$selected" >/dev/null 2>&1
      printf "Done.\n"
    fi
  fi
  ;;
"Theme")
  themelight=$(echo -e 'Dark\nLight' | rofi -dmenu -i -p "Select")

  case $themelight in "Dark")
    selected=$(echo -e 'Gruvbox\nOne\nNord\nDracula' | rofi -dmenu -i -p "Source")

    case $selected in "Gruvbox") theme="base16-gruvbox-hard";;
    "Nord") theme="base16-nord";;
    "One") theme="base16-onedark";;
    "Dracula") theme="base16-dracula";;
    *) printf "No option selected."
    esac

    wal --theme $theme
  ;;
  "Light")
    selected=$(echo -e 'Gruvbox' | rofi -dmenu -i -p "Source")

    case $selected in "Gruvbox") theme="base16-gruvbox-medium";;
    "Nord") theme="base16-nord";;
    "Dracula") theme="base16-dracula";;
    *) printf "No option selected."
    esac

    wal -l --theme $theme
  ;;
  esac


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
