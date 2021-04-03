"  _
" | | _____ _   _ _ __ ___   __ _ _ __  ___
" | |/ / _ \ | | | '_ ` _ \ / _` | '_ \/ __|
" |   <  __/ |_| | | | | | | (_| | |_) \__ \
" |_|\_\___|\__, |_| |_| |_|\__,_| .__/|___/
"           |___/                |_|
"

" Space as leader key
let mapleader=" "

nnoremap <leader>S :Startify<Enter>
nnoremap <leader>cm :CocList marketplace<Enter>
nnoremap <leader>e :CocCommand explorer<Enter>

" Resource config and restart CoC
nnoremap <leader>r :source ~/.config/nvim/init.vim<Enter> :CocRestart<Enter>


" Quit buffer
nnoremap <leader>q :q<Enter>

" Safe buffer
nnoremap <leader>w :w<Enter>

" Open a vertical split
nnoremap <leader>s :vsplit<Enter>

" Enable spellcheck
nnoremap <leader>es :set spell!<Enter>

" Move to begin/end of line with L and H
nnoremap <S-l> $
nnoremap <S-h> 0

" File specific auto pairs
autocmd Filetype tex let b:coc_pairs=[["$", "$"]]
autocmd Filetype markdown let b:coc_pairs=[["$", "$"]]

" Auto spell correction
inoremap <C-l> <Esc>[s1z=`]a


" Navigation and resizing in splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <A-h> :vertical resize +5<cr>
nnoremap <A-j> :resize +5<cr>
nnoremap <A-k> :resize -5<cr>
nnoremap <A-l> :vertical resize -5<cr>

" File-specific compile commands
augroup COMPILE_KEYMAPS
  " Clears all autocommands from this group
  autocmd!

  " Define the keybindings via autocommands
  autocmd Filetype c nmap <leader>c :!gcc -o output % -lm && alacritty --class Alacritty,Float -e ./output<Enter>
  autocmd Filetype cpp nmap <leader>c :!make && alacritty --class Alacritty,Float -e ./output<Enter>
  autocmd Filetype python nmap <leader>c :!alacritty --class Alacritty,Float -e python %<Enter>
  autocmd Filetype javascript nmap <leader>c :!node %<Enter>
  autocmd Filetype tex nmap <leader>c <plug>(vimtex-compile)
  autocmd FIletype tex nmap <leader>of :!zathura %:r.pdf&<Enter>
  autocmd Filetype pandoc nmap <leader>c :Pandoc pdf<Enter>
  autocmd FIletype pandoc nmap <leader>of :!zathura %:r.pdf&<Enter>
augroup END

" Latin long vowels there's probably a better way to do that
inoremap ä ā
inoremap Ä Ā
inoremap ë ē
inoremap Ë Ē
inoremap ï ī
inoremap Ï Ī
inoremap ö ō
inoremap Ö Ō
inoremap ü ū
inoremap Ü Ū
inoremap ÿ ȳ
inoremap Ÿ Ȳ
