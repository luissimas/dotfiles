#!/bin/zsh
screenshotPath="/home/padawan/dotfiles/.screenshots/"
obsidianPath="/home/padawan/dox/vault/Attachments/"

selected=$(echo -e 'Obsidian\nScreenshot' | rofi -dmenu -p "Type")

# Kill unclutter to allow mouse selection
killall unclutter

# Kill picom to avoid transparency bugs
# killall picom

# Launch scrot
if [[ "$selected" == "Obsidian" ]]; then
  filePath=$obsidianPath
  echo $filePath
  fileName=$(rofi -dmenu -p "File name" -l 0)
elif [[ "$selected" == "Screenshot" ]]; then
  filePath=$screenshotPath
  fileName=$(rofi -dmenu -p "File name" -l 0)
fi

maim -su $filePath$fileName.png

# Relaunch unclutter
unclutter&

# Relaunch picom
# picom&
