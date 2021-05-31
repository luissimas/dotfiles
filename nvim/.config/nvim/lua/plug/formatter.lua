require("formatter").setup({
  logging = false,
  filetype = {
    javascript = {
      -- prettier
      function()
        return { exe = "prettier", args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0) }, stdin = true }
      end
    },
    lua = {
      -- luafmt
      function()
        return {
          exe = "lua-format",
          args = {
            "--no-use-tab", "--tab-width", 2, "--indent-width", 2, "--column-limit", 120,
            "--spaces-inside-table-braces", "--chop-down-kv-table", "--no-keep-simple-function-one-line"
          },
          stdin = true
          -- exe = "luafmt",
          -- args = {"--indent-count", 2, "--stdin"},
          -- stdin = true
        }
      end
    },
    json = {
      -- jq
      function()
        return { exe = "jq", args = {}, stdin = true }
      end
    },
    python = {
      function()
        return { exe = "autopep8", args = { "--max-line-length", 120, vim.api.nvim_buf_get_name(0) }, stdin = true }
      end
    }
  }
})

-- Keymaps
vim.api.nvim_command(
    "autocmd FileType javascript,html,css,json,yaml,typescript,lua,python nnoremap <C-f> :FormatWrite<Enter>")
