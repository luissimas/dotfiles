"                        _                              __ _
"  _ __   ___  _____   _(_)_ __ ___     ___ ___  _ __  / _(_) __ _
" | '_ \ / _ \/ _ \ \ / / | '_ ` _ \   / __/ _ \| '_ \| |_| |/ _` |
" | | | |  __/ (_) \ V /| | | | | | | | (_| (_) | | | |  _| | (_| |
" |_| |_|\___|\___/ \_/ |_|_| |_| |_|  \___\___/|_| |_|_| |_|\__, |
"                                                            |___/
" Github: https://github.com/luissimas
"

"
" This is the main file init.vim, here we can source all the other config
" files. The idea is to keep things more modular and extensible.
"



"
" Base config files
"

" Vim-plug specific config file
source ~/.config/nvim/config/vim-plug.vim

" Colorschemes config
source ~/.config/nvim/config/colorschemes.vim

" All the keymaps
source ~/.config/nvim/config/keymaps.vim

" General neovim configuration via 'sets'
source ~/.config/nvim/config/sets.vim

" Autocommands
source ~/.config/nvim/config/autocommands.vim







"
" Plugin-specific configs
"
luafile ~/.config/nvim/plug/lspconfig.lua
luafile ~/.config/nvim/plug/galaxyline.lua
source ~/.config/nvim/plug/completion.vim
source ~/.config/nvim/plug/ultisnips.vim
source ~/.config/nvim/plug/tree.vim
source ~/.config/nvim/plug/airline.vim
source ~/.config/nvim/plug/vimtex.vim
source ~/.config/nvim/plug/pandoc.vim
