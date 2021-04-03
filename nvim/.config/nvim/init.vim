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
source ~/.config/nvim/vim-plug.vim

" Colorschemes config
source ~/.config/nvim/colorschemes.vim

" All the keymaps
source ~/.config/nvim/keymaps.vim

" General neovim configuration via 'sets'
source ~/.config/nvim/sets.vim

" Autocommands
source ~/.config/nvim/autocommands.vim





"
" Plugin-specific configs
"
source ~/.config/nvim/plug-config/coc.vim
source ~/.config/nvim/plug-config/startify.vim
source ~/.config/nvim/plug-config/airline.vim
source ~/.config/nvim/plug-config/whichkey.vim
source ~/.config/nvim/plug-config/vimtex.vim
source ~/.config/nvim/plug-config/pandoc.vim
source ~/.config/nvim/plug-config/texconceal.vim
