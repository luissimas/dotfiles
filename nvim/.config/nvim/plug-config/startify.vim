let g:startify_custom_header = 'startify#fortune#boxed()'
let g:startify_left_padding = 80
let g:startify_enable_special = 0
let g:startify_fortune_use_unicode = 1
let g:startify_files_number = 9
let g:startify_custom_indices = map(range(1,100), 'string(v:val)')
let g:startify_lists = [
  \ {'type': 'bookmarks', 'header': ['    --Bookmarks--']},
  \ {'type': 'files', 'header': ['    --Recent--']}
  \ ]
let g:startify_bookmarks = [
  \ {'vc': '~/.config/nvim/init.vim'},
  \ {'st': '~/repos/st/config.h'},
  \ {'dw': '~/repos/dwm/config.h'},
  \ {'zh': '~/.zshrc'},
  \ {'sc': '~/.scripts'},
  \ {'nt': '~/repos/notes'},
  \ {'ob': '~/repos/vault'},
  \ {'lt': '~/repos/notes/Latim/pensum'},
  \ {'js': '~/dox/projects'}
  \]

