-- Keymaps prefix
vim.g.vimwiki_map_prefix = "<leader>v"

-- Highlights for different header levels
vim.g.vimwiki_hl_headers = 0

-- Highlights for checked items
vim.g.vimwiki_hl_cb_checked = 2

-- Symbols for progression of todo items
vim.g.vimwiki_listsyms = " ○◐●✓"

-- Table auto formattting
vim.g.vimwiki_table_auto_fmt = 1

-- Conceal preformatted text
vim.g.vimwiki_conceal_pre = 0

-- Generate level 1 header on new pages
vim.g.vimwiki_auto_header = 1

-- Limit the plugin to only the files specified in vimwiki_list
vim.g.vimwiki_global_ext = 0

-- Have to look more into this stuff
vim.g.vimwiki_folding = ""

-- Each table in this table is a wiki
vim.g.vimwiki_list = {
  {
    path = "~/dox/vimwiki",
    name = "Main",
    syntax = "markdown",
    ext = ".md",
    maxhi = 1,
    automatic_nested_syntaxes = 1
  }
}
