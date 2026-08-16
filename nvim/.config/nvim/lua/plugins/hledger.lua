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
    "saghen/blink.compat",
    version = "2.*",
    lazy = true,
    opts = {},
  },
  {
    "saghen/blink.cmp",
    dependencies = {
      -- Vendored fork of kirasok/cmp-hledger (upstream ea2211c), patched to fix the
      -- off-by-one completion range that duplicated the first typed character.
      { dir = vim.fn.stdpath("config") .. "/plugins/cmp-hledger", name = "cmp-hledger" },
    },
    opts = {
      sources = {
        default = { "lsp", "path", "snippets", "buffer", "hledger" },
        providers = {
          hledger = {
            name = "hledger",
            module = "blink.compat.source",
            enabled = function()
              return vim.tbl_contains({ "ledger", "journal" }, vim.bo.filetype)
            end,
          },
        },
      },
    },
  },
}
