require("formatter").setup({
  logging = false,
  filetype = {
    javascript = {
      -- prettier
      function()
        return {
          exe = "prettier",
          args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0) },
          stdin = true,
        }
      end,
    },
    lua = {
      -- luafmt
      -- function()
      --   return {
      --     exe = "luafmt",
      --     args = {"--indent-count", 2, "--stdin"},
      --     stdin = true
      --   }
      -- end
      -- stylua
      function()
        return {
          exe = "stylua",
          args = {
            "--indent-type",
            "Spaces",
            "--indent-width",
            2,
            "--line-endings",
            "Unix",
            "--column-width",
            120,
            "--quote-style",
            "AutoPreferDouble",
            "-",
          },
          stdin = true,
        }
      end,
    },
    elixir = {
      function()
        return {
          exe = "mix",
          args = { "format -" },
          stdin = true,
        }
      end,
    },
    json = {
      -- jq
      function()
        return {
          exe = "jq",
          args = {},
          stdin = true,
        }
      end,
    },
    python = {
      function()
        return {
          exe = "autopep8",
          args = { "--max-line-length", 120, vim.api.nvim_buf_get_name(0) },
          stdin = true,
        }
      end,
    },
  },
})

-- Keymaps
vim.api.nvim_command(
  "autocmd FileType javascript,html,css,json,yaml,typescript,lua,python nnoremap <C-f> :FormatWrite<Enter>"
)
