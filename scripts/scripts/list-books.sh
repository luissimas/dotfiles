#!/bin/bash

booksDirectory=/home/padawan/repos/books

function get_books() {
    ls $wallpaperDirectory
}

selected=$(ls -R $booksDirectory | grep '.pdf\|.epub\|.djvu' | sed 's/\/home\/padawan\/repos\/books\///' | rofi -dmenu -i -p "Select a file")

if [ -z "$selected" ]; then
    echo "No book selected"
else
    zathura $booksDirectory/$selected
fi
