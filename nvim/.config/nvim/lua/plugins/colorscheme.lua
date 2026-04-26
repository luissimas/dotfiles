return {
  { "ellisonleao/gruvbox.nvim", lazy = false, priority = 1000, opts = { contrast = "hard" } },
  { "projekt0n/github-nvim-theme", lazy = false, priority = 1000, name = "github-theme" },
  { "folke/tokyonight.nvim", lazy = false, priority = 1000 },
  { "catppuccin/nvim", lazy = false, priority = 1000, name = "catppuccin" },
  { "rose-pine/neovim", lazy = false, priority = 1000, name = "rose-pine" },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = function()
        require("config.theme").apply()
      end,
    },
  },
}
