set timeoutlen=500

let g:which_key_sep='->'
let g:which_key_use_floating_win=0

" hide status bar
autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

let g:which_key_map = {}

let g:which_key_map.l = {
            \ 'name':"+LaTeX",
            \ '<Tab>':"Hello world",
            \ 'c':"Clean files"
            \}

let g:which_key_map.b = {
  \ 'name':"+File",
  \ 's':"save"
  \ }

nnoremap <silent> <leader> :<C-U>WhichKey '<Space>'<CR>
