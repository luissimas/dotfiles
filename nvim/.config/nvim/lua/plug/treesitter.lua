--
--  _                      _ _   _
-- | |_ _ __ ___  ___  ___(_) |_| |_ ___ _ __
-- | __| '__/ _ \/ _ \/ __| | __| __/ _ \ '__|
-- | |_| | |  __/  __/\__ \ | |_| ||  __/ |
--  \__|_|  \___|\___||___/_|\__|\__\___|_|
--
--

require("nvim-treesitter.configs").setup {
  -- List of language parsers to always have installed
  ensure_installed = "maintained",
  indent = {
    enable = true
  },
  highlight = {
    enable = true
  },
  -- Color pairs
  rainbow = {
    enable = true,
    extended_mode = true,
    max_file_lines = 1000,
    colors = {
      "#f7768e",
      "#e0af68",
      "#bb9af7",
      "#7dcfff"
    }
  }
}
