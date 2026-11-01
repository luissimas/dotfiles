return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        ledger = { "trim_newlines", "trim_whitespace" },
      },
    },
  },
  {
    "saghen/blink.cmp",
    opts = {
      sources = {
        default = { "lsp", "path", "snippets", "buffer", "hledger" },
        providers = {
          hledger = {
            name = "hledger",
            module = "hledger_source",
            -- Rank accounts above buffer words
            score_offset = 10,
          },
        },
      },
    },
  },
}
