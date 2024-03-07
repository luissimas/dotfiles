#!/usr/bin/env bash
screenshotPath="/home/padawan/dotfiles/.screenshots/"
notesPath="/home/padawan/repos/zettelkasten/attachments/"

selected=$(echo -e 'Notes\nScreenshot' | rofi -dmenu -p "Type")

# Kill unclutter to allow mouse selection
killall unclutter

# Kill picom to avoid transparency bugs
# killall picom

# Launch scrot
if [[ "$selected" == "Notes" ]]; then
    filePath=$notesPath
    echo $filePath
    fileName=$(rofi -dmenu -p "File name" -l 0)
elif [[ "$selected" == "Screenshot" ]]; then
    filePath=$screenshotPath
    fileName=$(rofi -dmenu -p "File name" -l 0)
fi

maim -su $filePath$fileName.png

# Relaunch unclutter
unclutter &

# Relaunch picom
# picom&
