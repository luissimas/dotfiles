"
"              _                                                        _
"   __ _ _   _| |_ ___     ___ ___  _ __ ___  _ __ ___   __ _ _ __   __| |___
"  / _` | | | | __/ _ \   / __/ _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
" | (_| | |_| | || (_) | | (_| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
"  \__,_|\__,_|\__\___/   \___\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/
"




" Trim white spaces on buffer writes
fun! TrimWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun

augroup TRIMWHITESPACE
  " Clears all autocommands from this group
  autocmd!

  " Defines autocmd to call the TrimWhitespace function
  " right before the writting of a buffer
  autocmd BufWritePre * :call TrimWhitespace()
augroup END
