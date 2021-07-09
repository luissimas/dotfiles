local whichkey = require("which-key")

-- Main setup
whichkey.setup {
  plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    spelling = {
      enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20 -- how many suggestions should be shown in the list?
    },
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    presets = {
      operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = false, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true -- bindings for prefixed with g
    }
  },
  -- add operators that will trigger motion and text object completion
  -- to enable all native operators, set the preset / operators plugin above
  operators = {gc = "Comments"},
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "➜", -- symbol used between a key and it's label
    group = "+" -- symbol prepended to a group
  },
  window = {
    border = "none", -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin = {1, 0, 1, 0}, -- extra window margin [top, right, bottom, left]
    padding = {1, 1, 1, 1} -- extra window padding [top, right, bottom, left]
  },
  layout = {
    height = {min = 3, max = 10}, -- min and max height of the columns
    width = {min = 20, max = 50}, -- min and max width of the columns
    spacing = 10 -- spacing between columns
  },
  ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
  hidden = {"<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
  show_help = false, -- show help message on the command line when the popup is visible
  triggers = "auto" -- automatically setup triggers
  -- triggers = {"<leader>"} -- or specifiy a list manually
}

-- Mapping keys
whichkey.register(
  {
    ["<leader>"] = {
      b = {
        name = "Buffer",
        d = {"<cmd>bp <bar> vs <bar> bn <bar> bd <Enter>", "Kill buffer"},
        l = {"", "Switch to last buffer"}
      },
      [","] = {
        '<cmd>lua require("telepada").switch_buffer()<Enter>',
        "Switch buffer"
      },
      c = {
        name = "Code",
        r = {"<cmd> Lspsaga rename<Enter>", "Rename"},
        a = {"<cmd> Lspsaga code_action<Enter>", "Code actions"},
        d = {"<cmd> Lspsaga show_line_diagnostics<Enter>", "Line diagnostics"}
      },
      s = {
        name = "Spell",
        k = {"<cmd>set spell!<Enter>", "Toggle spellcheck"}
      },
      t = {
        name = "Trouble",
        w = {"<cmd>TroubleToggle lsp_workspace_diagnostics<Enter>", "Lsp workspace diagnostics"},
        d = {"<cmd>TroubleToggle lsp_document_diagnostics<Enter>", "Lsp document diagnostics"},
        q = {"<cmd>TroubleToggle quickfix<Enter>", "Quickfix list"},
        l = {"<cmd>TroubleToggle loclist<Enter>", "Local list"}
      },
      h = {"<cmd>Telescope help_tags<Enter>", "Help"},
      f = {
        name = "Find",
        g = {"<cmd>Telescope live_grep<Enter>", "Live grep"},
        r = {"<cmd>Telescope oldfiles", "Recent files"},
        p = {"<cmd>lua require('telescope').extensions.project.project({})<Enter>", "Find projects"},
        f = {"<cmd>lua require('telepada').find_files()<Enter>", "Find files"},
        b = {"<cmd>lua require('telepada').find_buffer()<Enter>", "Search buffer"},
        d = {'<cmd>lua require("telepada").find_dotfiles()<Enter>', "Find in dotfiles"},
        v = {'<cmd>lua require("telepada").find_vault()<Enter>', "Find in vault"},
        c = {'<cmd>lua require("telepada.colorscheme").colorscheme()<Enter>', "Change colorscheme"},
        w = {'<cmd>lua require("telepada.colorscheme").wal()<Enter>', "Change pywal colorscheme"}
      },
      w = {"<cmd>w<Enter>", "Write buffer"},
      q = {"<cmd>q<Enter>", "Quit"},
      g = {"<cmd>lua require('neogit').open()<Enter>", "Neogit"},
      -- g = {"<cmd>Git<Enter>", "Fugitive"},
      e = {"<cmd>NvimTreeToggle<Enter>", "Toggle tree"}
    }
  }
)
