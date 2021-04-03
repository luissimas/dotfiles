"
"  _
" | |_ _ __ ___  ___
" | __| '__/ _ \/ _ \
" | |_| | |  __/  __/
"  \__|_|  \___|\___|
"
"


" Tree side
let g:nvim_tree_side = 'left'

" Tree width
let g:nvim_tree_width = 25

" Update cursor when entering a buffer
let g:nvim_tree_follow = 1


" Files to be ignored
let g:nvim_tree_ignore = [ '.git', 'node_modules', '.cache' ]

" Automatically close tree buffer when a file is opened
let g:nvim_tree_quit_on_open = 1

" Automatically close tree buffer when it's the last buffer
let g:nvim_tree_auto_close = 1

" Icons to show
let g:nvim_tree_show_icons = { 'git': 1, 'folders': 1, 'files': 1 }

" Highlight for git attributes
let g:nvim_tree_git_hl = 1

" Keeping netrw (it's useful for some other stuff
let g:nvim_tree_disable_netrw = 0 "1 by default, disables netrw

" Icons
let g:nvim_tree_icons = {
    \ 'default': '',
    \ 'symlink': '',
    \ 'git': {
    \   'unstaged': "✗",
    \   'staged': "✓",
    \   'unmerged': "",
    \   'renamed': "➜",
    \   'untracked': "★",
    \   'deleted': ""
    \   },
    \ 'folder': {
    \   'default': "",
    \   'open': "",
    \   'empty': "",
    \   'empty_open': "",
    \   'symlink': "",
    \   'symlink_open': "",
    \   }
    \ }
