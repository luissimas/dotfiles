require("formatter").setup(
  {
    logging = false,
    filetype = {
      javascript = {
        -- prettier
        function()
          return {
            exe = "prettier",
            args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0)},
            stdin = true
          }
        end
      },
      lua = {
        -- luafmt
        function()
          return {
            exe = "luafmt",
            args = {"--indent-count", 2, "--stdin"},
            stdin = true
          }
        end
      },
      elixir = {
        function()
          return {
            exe = "mix format",
            args = {},
            stdin = false
          }
        end
      },
      json = {
        -- jq
        function()
          return {
            exe = "jq",
            args = {},
            stdin = true
          }
        end
      },
      python = {
        function()
          return {
            exe = "autopep8",
            args = {"--max-line-length", 120, vim.api.nvim_buf_get_name(0)},
            stdin = true
          }
        end
      }
    }
  }
)

-- Keymaps
vim.api.nvim_command(
  "autocmd FileType javascript,html,css,json,yaml,typescript,lua,python nnoremap <C-f> :FormatWrite<Enter>"
)
