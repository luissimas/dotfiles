return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    lazy = false,
    build = ":TSUpdate",
    opts = {
      install_dir = vim.fn.stdpath("data") .. "/site",
    },
    init = function()
      require("nvim-treesitter").install({
        "bash",
        "bicep",
        "c",
        "diff",
        "dockerfile",
        "eex",
        "elixir",
        "gitcommit",
        "go",
        "gomod",
        "gosum",
        "html",
        "javascript",
        "jsdoc",
        "json",
        "latex",
        "ledger",
        "lua",
        "luadoc",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "xml",
        "yaml",
      })
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    enabled = true,
    branch = "main",
    opts = {
      move = {
        enable = true,
        set_jumps = false,
        keys = {
          goto_next_start = {
            ["]c"] = "@code_block.inner",
          },
          goto_previous_start = {
            ["[c"] = "@code_block.inner",
          },
        },
      },
    },
  },
  {
    "nvim-mini/mini.ai",
    opts = function(_, opts)
      local ai = require("mini.ai")
      opts.custom_textobjects = vim.tbl_deep_extend("force", opts.custom_textobjects or {}, {
        k = ai.gen_spec.treesitter({
          a = "@code_block.outer",
          i = "@code_block.inner",
        }),
      })
    end,
  },
}
