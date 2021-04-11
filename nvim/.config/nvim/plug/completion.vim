"
"                            _      _   _
"   ___ ___  _ __ ___  _ __ | | ___| |_(_) ___  _ __
"  / __/ _ \| '_ ` _ \| '_ \| |/ _ \ __| |/ _ \| '_ \
" | (_| (_) | | | | | | |_) | |  __/ |_| | (_) | | | |
"  \___\___/|_| |_| |_| .__/|_|\___|\__|_|\___/|_| |_|
"                     |_|
"




" Options
set completeopt=menuone,noinsert,noselect

" Matching strategy
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']

" Enabling UltiSnips
let g:completion_enable_snippet = 'UltiSnips'

" Enabling lspkind icons
lua require('lspkind').init()


" Enabling completion on every markdown buffer
" This is required for my setup since that at the moment I
" don't have any lsp server to attach the completion
" plugin for those files, but I still need the completion
" to show my snippets
autocmd Filetype markdown lua require('completion').on_attach()
autocmd Filetype rmd lua require('completion').on_attach()
