" Key maps
nnoremap <leader>r :source ~/.config/nvim/init.vim<Enter> :CocRestart<Enter>
nnoremap <leader>q :q<Enter>
nnoremap <leader>w :w<Enter>
nnoremap <leader>s :vsplit<Enter>
nnoremap <leader>es :set spell!<Enter>
nnoremap <leader>S :Startify<Enter>
nnoremap <leader>cm :CocList marketplace<Enter>
nnoremap <leader>e :CocCommand explorer<Enter>
nnoremap <silent> <Leader>/ :noh<cr>

" File specific auto pairs
autocmd Filetype tex let b:coc_pairs=[["$", "$"]]
autocmd Filetype markdown let b:coc_pairs=[["$", "$"]]

" Auto spell correction
inoremap <C-l> <Esc>[s1z=`]a

" Compile outputs or show previews
autocmd Filetype c nmap <leader>c :!gcc -o output % -lm && alacritty --class Alacritty,Float -e ./output<Enter>
autocmd Filetype javascript nmap <leader>c :!node %<Enter>
autocmd Filetype tex nmap <leader>c <plug>(vimtex-compile)
nmap <leader>c :Pandoc pdf<Enter>
nmap <leader>of :!zathura %:r.pdf&<Enter>

" Navigation and resizing in splits
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <A-h> :vertical resize +5<cr>
nnoremap <A-j> :resize +5<cr>
nnoremap <A-k> :resize -5<cr>
nnoremap <A-l> :vertical resize -5<cr>

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
