require("formatter").setup(
  {
    logging = false,
    filetype = {
      javascript = {
        -- prettier
        function()
          return {
            exe = "prettier",
            args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0), "--single-quote"},
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
            args = {"--max-line-length", 120, "--aggressive"},
            stdin = true
          }
        end
      }
    }
  }
)
