"
"               _                                                        _
"    __ _ _   _| |_ ___     ___ ___  _ __ ___  _ __ ___   __ _ _ __   __| |___
"   / _` | | | | __/ _ \   / __/ _ \| '_ ` _ \| '_ ` _ \ / _` | '_ \ / _` / __|
"  | (_| | |_| | || (_) | | (_| (_) | | | | | | | | | | | (_| | | | | (_| \__ \
"   \__,_|\__,_|\__\___/   \___\___/|_| |_| |_|_| |_| |_|\__,_|_| |_|\__,_|___/
"
"  To find filetype use :set filetype?
"

" Trim white spaces on buffer writes
fun! TrimWhitespace()
  let l:save = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:save)
endfun

augroup Markdown
  autocmd!
  autocmd Filetype markdown :set nowrap&
  autocmd Filetype pandoc :set nowrap&

  autocmd Filetype markdown :nnoremap j gj
  autocmd Filetype markdown :nnoremap k gk
augroup END

" Hidding tabline in vimwiki buffers
autocmd Filetype vimwiki :setlocal showtabline=0

" Packer autocompile
autocmd BufWritePost plugins.lua luafile %

augroup TRIMWHITESPACE
  " Clears all autocommands from this group
  autocmd!

  " Defines autocmd to call the TrimWhitespace function
  " right before the writting of a buffer
  autocmd BufWritePre * :call TrimWhitespace()
augroup END

augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.js,*.rs,*.lua,*.py,*.json,*.exs,*.ex,*.h,*.c,*.cpp,*.ml silent FormatWrite
augroup END

augroup ElixirFiletype
  autocmd!
  autocmd BufRead,BufNewFile *.ex,*.exs set filetype=elixir
  autocmd BufRead,BufNewFile *.eex set filetype=eelixir
augroup END

augroup ErlangFiletype
  autocmd!
  autocmd BufRead,BufNewFile *.yrl,*.xrl set filetype=erlang
augroup END

augroup STATUSLINECOLORSYNC
  " Clears all autocommands from this group
  autocmd!

  " On colorscheme change, change the statusline theme as well
  " autocmd ColorScheme * :exe':AirlineTheme '.g:colors_name
augroup END

" Disable automatic comment insertion
autocmd BufEnter * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" File-specific compile commands
augroup COMPILE_KEYMAPS
  autocmd!

  autocmd Filetype c nmap <leader>o :!make && kitty --class Float -e ./output<Enter>
  autocmd Filetype cpp nmap <leader>o :!make && kitty --class Float -e ./output<Enter>
  autocmd Filetype python nmap <leader>o :!kitty --class Float -e python %<Enter>
  autocmd Filetype javascript nmap <leader>o :!node %<Enter>
  autocmd Filetype tex nmap <leader>o <plug>(vimtex-compile)
  autocmd Filetype tex nmap <leader>of :!zathura %:r.pdf&<Enter>

  autocmd Filetype pandoc nmap <leader>o :Pandoc pdf<Enter>
  autocmd Filetype pandoc nmap <leader>of :!zathura %:r.pdf&<Enter>

  autocmd Filetype markdown nnoremap <silent> <leader>of :!zathura %:r.pdf&<Enter><Enter>
  " autocmd Filetype markdown nnoremap <leader>o :Pandoc pdf<Enter>

  autocmd Filetype rmd nmap <silent> <leader>o :!Rscript -e "rmarkdown::render('%')" >/dev/null<Enter>
  autocmd Filetype rmd nmap <silent> <leader>of :!zathura %:r.pdf&<Enter><Enter>

  autocmd Filetype sql nmap <leader>o <Plug>(DBUI_ExecuteQuery)
  autocmd Filetype sql vmap <leader>o <Plug>(DBUI_ExecuteQuery)

augroup END

" Conceal
augroup CONCEAL
  autocmd!
  autocmd Filetype elixir syntax keyword Identifier fn conceal cchar=λ
augroup END
