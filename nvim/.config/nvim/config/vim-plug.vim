"        _                       _
" __   _(_)_ __ ___        _ __ | |_   _  __ _
" \ \ / / | '_ ` _ \ _____| '_ \| | | | |/ _` |
"  \ V /| | | | | | |_____| |_) | | |_| | (_| |
"   \_/ |_|_| |_| |_|     | .__/|_|\__,_|\__, |
"                         |_|            |___/
"

" Automatically install vim-plug itself on startup
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif



call plug#begin('~/.config/nvim/autoload/plugged')
  " LSP Config
  Plug 'neovim/nvim-lspconfig'

  " Autocompletion
  Plug 'nvim-lua/completion-nvim'

  " Lspking completion popup icons
  Plug 'onsails/lspkind-nvim'

  " Treesitter
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

  " Snippets
  Plug 'SirVer/ultisnips'

  " File tree
  Plug 'kyazdani42/nvim-tree.lua'

  " Galaxyline statusbar
  Plug 'glepnir/galaxyline.nvim' , {'branch': 'main'}

  " Bufferline
  Plug 'akinsho/nvim-bufferline.lua'

  " Icons for file tree and statusbar
  Plug 'kyazdani42/nvim-web-devicons'





  " Vimtex for latex
  Plug 'lervag/vimtex'

  " Vim-pandoc for makdown compile and highlight
  Plug 'vim-pandoc/vim-pandoc'
  Plug 'vim-pandoc/vim-pandoc-syntax'

  " Colorschemes
  Plug 'morhetz/gruvbox'
  Plug 'dylanaraps/wal.vim'
  Plug 'joshdick/onedark.vim'
  Plug 'arcticicestudio/nord-vim'
  Plug 'dracula/vim', { 'as': 'dracula' }

call plug#end()