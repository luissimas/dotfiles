--
--  _                      _ _   _
-- | |_ _ __ ___  ___  ___(_) |_| |_ ___ _ __
-- | __| '__/ _ \/ _ \/ __| | __| __/ _ \ '__|
-- | |_| | |  __/  __/\__ \ | |_| ||  __/ |
--  \__|_|  \___|\___||___/_|\__|\__\___|_|
--
--



require('nvim-treesitter.configs').setup {
  -- List of language parsers to always have installed
   ensure_installed = {
     "r",
     "c",
     "cpp",
     "css",
     "tsx",
     "lua",
     "json",
     "bash",
     "html",
     "yaml",
     "regex",
     "latex",
     "python",
     "javascript",
     "typescript"
   },

  highlight = {
    enable = true
  },

  -- Color pairs
  rainbow = {
    enable = true,
  },
}
