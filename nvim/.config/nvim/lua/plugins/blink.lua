return {
  {
    "saghen/blink.cmp",
    dependencies = {
      { "Kaiser-Yang/blink-cmp-dictionary", dependencies = { "nvim-lua/plenary.nvim" } },
    },
    opts = {
      completion = {
        ghost_text = {
          enabled = false,
        },
      },
      sources = {
        per_filetype = {
          markdown = {
            "lsp",
            "dictionary",
          },
        },
        providers = {
          dictionary = {
            name = "Dict",
            module = "blink-cmp-dictionary",
            min_keyword_length = 3,
            opts = {
              dictionary_files = { "/usr/share/dict/words" },
            },
          },
        },
      },
    },
  },
}
