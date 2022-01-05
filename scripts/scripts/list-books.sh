#!/bin/zsh

booksDirectory=/home/padawan/dox/books

function get_books(){
  ls $wallpaperDirectory
}

selected=$(ls $booksDirectory/**/* | grep '.pdf\|.epub\|.djvu' | sed 's/\/home\/padawan\/dox\/books\///' | dmenu -i -p "Select a file")

if [ -z "$selected" ]; then
  echo "No book selected"
else
  zathura $booksDirectory/$selected
fi

