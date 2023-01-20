#!/bin/bash

booksDirectory=/home/padawan/repos/books/

selected=$(fd . $booksDirectory -t f | sed 's/\/home\/padawan\/repos\/books\///' | rofi -dmenu -i -p "Select a file")

if [ -z "$selected" ]; then
    echo "No book selected"
else
    zathura $booksDirectory/"$selected"
fi
