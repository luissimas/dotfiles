--
--              _                                                        _
--   __ _ _   _| |_ ___     ___ ___  _ __ ___  _ __ ___   __ _ _ __   __| |___
--  / _` | | | | __/ _ \   / __/ _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
-- | (_| | |_| | || (_) | | (_| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
--  \__,_|\__,_|\__\___/   \___\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/
--
-- To find filetype use :set filetype?
--

vim.api.nvim_exec([[

" Trim white spaces on buffer writes
fun! TrimWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun

autocmd Filetype markdown :set nowrap&

augroup TRIMWHITESPACE
  " Clears all autocommands from this group
  autocmd!

  " Defines autocmd to call the TrimWhitespace function
  " right before the writting of a buffer
  autocmd BufWritePre * :call TrimWhitespace()
augroup END

augroup STATUSLINECOLORSYNC
  " Clears all autocommands from this group
  autocmd!

  " On colorscheme change, change the statusline theme as well
  " autocmd ColorScheme * :exe':AirlineTheme '.g:colors_name
augroup END


" File-specific compile commands
augroup COMPILE_KEYMAPS
autocmd!

autocmd Filetype c nmap <leader>o :!gcc -o output % -lm && alacritty --class Alacritty,Float -e ./output<Enter>
autocmd Filetype cpp nmap <leader>o :!make && alacritty --class Alacritty,Float -e ./output<Enter>
autocmd Filetype python nmap <leader>o :!alacritty --class Alacritty,Float -e python %<Enter>
autocmd Filetype javascript nmap <leader>o :!node %<Enter>
autocmd Filetype tex nmap <leader>o <plug>(vimtex-compile)
autocmd Filetype tex nmap <leader>of :!zathura %:r.pdf&<Enter>

autocmd Filetype pandoc nmap <leader>o :Pandoc pdf<Enter>
autocmd Filetype pandoc nmap <leader>of :!zathura %:r.pdf&<Enter>

autocmd Filetype rmd nmap <silent> <leader>o :!Rscript -e \"rmarkdown::render('%')\" >/dev/null<Enter><>
autocmd Filetype rmd nmap <silent> <leader>of :!zathura %:r.pdf&<Enter><Enter>

augroup END

]], false)
