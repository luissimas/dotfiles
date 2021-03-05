" 
"  _ __   ___  _____   _(_)_ __ ___  
" | '_ \ / _ \/ _ \ \ / / | '_ ` _ \ 
" | | | |  __/ (_) \ V /| | | | | | |
" |_| |_|\___|\___/ \_/ |_|_| |_| |_|
"
"
" Github: https://github.com/luissimas 
"

let mapleader="\<space>"

" Auto-install vim-plug and missing plugins
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  "autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif



" Import plugins and source configs
call plug#begin('~/.config/nvim/autoload/plugged')

    " CoC Intellisense, autopairs, file tree etc. Basically the core of this config
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
    source ~/.config/nvim/plug-config/coc.vim

    " Startify for a start page
    Plug 'mhinz/vim-startify'
    source ~/.config/nvim/plug-config/startify.vim

    " Better syntax highlight
    Plug 'sheerun/vim-polyglot'	 

    " Airline statusbar
    Plug 'vim-airline/vim-airline'
    source ~/.config/nvim/plug-config/airline.vim

    " Wich key for keybinding display
    Plug 'liuchengxu/vim-which-key'
    source ~/.config/nvim/plug-config/whichkey.vim

    " Vimtex for latex
    Plug 'lervag/vimtex'
    source ~/.config/nvim/plug-config/vimtex.vim

    " Vim-pandoc for makdown compile and highlight
    Plug 'vim-pandoc/vim-pandoc'
    Plug 'vim-pandoc/vim-pandoc-syntax' 
    source ~/.config/nvim/plug-config/pandoc.vim

    " Tex-conceal for latex
    Plug 'KeitaNakamura/tex-conceal.vim', {'for': 'tex'}
    source ~/.config/nvim/plug-config/texconceal.vim

    " Snippets package
    " Plug 'honza/vim-snippets'

    " Colorschemes
    Plug 'morhetz/gruvbox'
    Plug 'colepeters/spacemacs-theme.vim'
    Plug 'dracula/vim', {'name':'dracula'}
    Plug 'romgrk/doom-one.vim'
    Plug 'dylanaraps/wal.vim'
    source ~/.config/nvim/colorschemes.vim

call plug#end()

let g:airline_theme = 'wal'
"let g:airline_theme = 'gruvbox'

colorscheme wal
"colorscheme gruvbox

" Spellcheck
set spelllang=pt,en,la

"General config
set number relativenumber
set encoding=utf8
set smartindent
set autoindent
set linebreak
set splitbelow
set splitright
set updatetime=1000
set nohlsearch
set incsearch
set scrolloff=8
set nowrap
set foldmethod=indent
set foldnestmax=10
set nofoldenable
set foldlevel=2



" Tabs
set tabstop=2
set shiftwidth=2
set expandtab

" Clipboard stuff
set clipboard=unnamedplus

" Disable netrw
"let g:loaded_netrw=1
"let g:loaded_netrwPlugin=1

" Keymaps
source ~/.config/nvim/keymaps.vim
