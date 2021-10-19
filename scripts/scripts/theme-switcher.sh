#!/bin/zsh

wallpaperDirectory=/home/padawan/.wal/

# Prompt if the script should use the wallpaper or the themes
option=$(echo -e 'Theme\nWallpaper\nPywal' | rofi -dmenu -i -p "Source")

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
      bspwmcolors.sh

      # Update betterlockscreen (takes a while)
      printf "Setting lock screen...\n"
      betterlockscreen -u "$wallpaperDirectory$selected" >/dev/null 2>&1
      printf "Done.\n"
    fi
  fi
  ;;
"Theme")
  selected=$(echo -e 'Palenight\nTokyonight\nRosé Pine' | rofi -dmenu -i -p "Source")

  case $selected in "Palenight")
    theme="base16-material-palenight"
    feh --bg-fill ~/.wal/palenight.jpg
    ;;
  "Rosé Pine")
    theme=~/.config/wal/colorschemes/rosepine.json
    feh --bg-fill ~/.wal/sunset.png
    ;;
  "Tokyonight")
    theme=~/.config/wal/colorschemes/tokyonight.json
    feh --bg-fill ~/.wal/traintunnel.jpg
    ;;
  *) printf "No option selected."
  esac

  wal --theme $theme

  # Update bspwm colors
  bspwmcolors.sh

  # Update qutebrowser colors
  pgrep qutebrowser > /dev/null && qutebrowser :config-source
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
