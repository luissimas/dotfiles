"            _                     _
"   ___ ___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___  ___
"  / __/ _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \/ __|
" | (_| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/\__ \
"  \___\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___||___/
"
"

" Gruvbox specific settings
let g:gruvbox_italic=1
let g:gruvbox_bold=1
let g:gruvbox_contrast_dark = "hard"
let g:gruvbox_termcolors=16


lua vim.cmd('colorscheme '..require('colorscheme').colorscheme)
