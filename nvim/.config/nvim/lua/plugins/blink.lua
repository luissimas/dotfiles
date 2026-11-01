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
            "snippets",
            "dictionary",
          },
        },
        providers = {
          snippets = {
            -- Rank snippets above dictionary words in markdown
            score_offset = function(ctx)
              return vim.bo[ctx.bufnr].filetype == "markdown" and 10 or -1
            end,
          },
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
