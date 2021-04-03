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


" Keymappings
lua <<EOF
    local tree_cb = require'nvim-tree.config'.nvim_tree_callback

    vim.g.nvim_tree_bindings = {
      ["<C-t>"]          = tree_cb("tabnew"),
      ["<CR>"]           = tree_cb("cd"),
      ["o"]              = tree_cb("edit"),
      ["l"]              = tree_cb("edit"),
      ["s"]              = tree_cb("vsplit"),
      ["h"]              = tree_cb("dir_up"),
      ["I"]              = tree_cb("toggle_ignored"),
      ["H"]              = tree_cb("toggle_dotfiles"),
      ["R"]              = tree_cb("refresh"),
      ["a"]              = tree_cb("create"),
      ["d"]              = tree_cb("remove"),
      ["r"]              = tree_cb("rename"),
      ["x"]              = tree_cb("cut"),
      ["c"]              = tree_cb("copy"),
      ["p"]              = tree_cb("paste"),
      ["q"]              = tree_cb("close"),
    }
EOF
