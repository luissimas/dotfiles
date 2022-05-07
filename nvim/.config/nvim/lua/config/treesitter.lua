require("nvim-treesitter.configs").setup {
  ensure_installed = {"c", "lua", "elixir", "ocaml", "erlang", "haskell", "html", "javascript", "jsdoc", "json", "latex", "ocaml_interface", "org", "python", "regex", "typescript", "vim"},
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  rainbow = {
    enable = true,
    extended_mode = true,
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
}
