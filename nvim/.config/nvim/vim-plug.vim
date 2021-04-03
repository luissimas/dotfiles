"        _                       _             
" __   _(_)_ __ ___        _ __ | |_   _  __ _ 
" \ \ / / | '_ ` _ \ _____| '_ \| | | | |/ _` |
"  \ V /| | | | | | |_____| |_) | | |_| | (_| |
"   \_/ |_|_| |_| |_|     | .__/|_|\__,_|\__, |
"                         |_|            |___/ 
" 

" Automatically install vim-plug itself on startup
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif



call plug#begin('~/.config/nvim/autoload/plugged')

    " CoC Intellisense, autopairs, file tree etc. Basically the core of this config
    Plug 'neoclide/coc.nvim', {'branch': 'release'}

    " Startify for a start page
    Plug 'mhinz/vim-startify'

    " Better syntax highlight
    Plug 'sheerun/vim-polyglot'	 

    " Airline statusbar
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'

    " Wich key for keybinding display
    Plug 'liuchengxu/vim-which-key'

    " Vimtex for latex
    Plug 'lervag/vimtex'

    " Vim-pandoc for makdown compile and highlight
    Plug 'vim-pandoc/vim-pandoc'
    Plug 'vim-pandoc/vim-pandoc-syntax' 

    " Tex-conceal for latex
    Plug 'KeitaNakamura/tex-conceal.vim', {'for': 'tex'}

    " Colorschemes
    Plug 'romgrk/doom-one.vim'
    Plug 'morhetz/gruvbox'
    Plug 'dylanaraps/wal.vim'
    Plug 'arcticicestudio/nord-vim'

call plug#end()
