require("project_nvim").setup {
  patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json", "mix.exs", "dune" },
  ignore_lsp = { "null-ls" },
  show_hidden = true,
}
