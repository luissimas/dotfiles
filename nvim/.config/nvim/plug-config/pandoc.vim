" Define filetypes
let g:pandoc#filetypes#handled = ["pandoc", "markdown"]
let g:pandoc#command#prefer_pdf = 1

" Disable some modules that I don't use
let g:pandoc#modules#disabled = ["folding", "templates", "spell"]
let g:pandoc#spell#enabled = 0

" Autocompile on save
" let g:pandoc#command#autoexec_on_writes = 1
" let g:pandoc#command#autoexec_command = "Pandoc pdf"
