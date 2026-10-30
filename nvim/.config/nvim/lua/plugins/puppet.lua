return {
  "stevearc/conform.nvim",
  opts = {
    formatters_by_ft = {
      puppet = { "puppet-lint" },
    },
    formatters = {
      -- Use the puppet-lint bundled with Mason's puppet-editor-services
      ["puppet-lint"] = {
        command = vim.fn.stdpath("data") .. "/mason/packages/puppet-editor-services/libexec/vendor/puppet-lint/bin/puppet-lint",
      },
    },
  },
}
