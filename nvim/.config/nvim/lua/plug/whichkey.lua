local whichkey = require("which-key")

-- Main setup
whichkey.setup {
  plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = false, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
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
    height = {min = 4, max = 25}, -- min and max height of the columns
    width = {min = 20, max = 50}, -- min and max width of the columns
    spacing = 3 -- spacing between columns
  },
  ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
  hidden = {"<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
  show_help = true, -- show help message on the command line when the popup is visible
  triggers = "auto" -- automatically setup triggers
  -- triggers = {"<leader>"} -- or specifiy a list manually
}

-- Mapping keys
whichkey.register(
  {
    ["<leader>"] = {
      -- Telescope
      f = {
        name = "Telescope",
        f = {"<cmd>Telescope find_files<Enter>", "Find files"},
        g = {"<cmd>Telescope live_grep<Enter>", "Grep"},
        h = {"<cmd>Telescope help_tags<Enter>", "Help"},
        b = {
          "<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find({ sorting_strategy='ascending', prompt_position = 'top'})<Enter>",
          "Current buffer"
        },
        d = {'<cmd>lua require("telescope_custom.find").find_dotfiles()<Enter>', "Find dotfiles"},
        v = {'<cmd>lua require("telescope_custom.find").find_vault()<Enter>', "Find vault"},
        c = {'<cmd>lua require("telescope_custom.colorscheme").colorscheme()<Enter>', "Colorscheme"},
        w = {'<cmd>lua require("telescope_custom.colorscheme").wal()<Enter>', "Pywal"}
      },
      -- Trouble
      t = {
        name = "Trouble",
        w = {"<cmd>TroubleToggle lsp_workspace_diagnostics<Enter>", "Lsp workspace diagnostics"},
        d = {"<cmd>TroubleToggle lsp_document_diagnostics<Enter>", "Lsp document diagnostics"},
        q = {"<cmd>TroubleToggle quickfix<Enter>", "Quickfix list"},
        l = {"<cmd>TroubleToggle loclist<Enter>", "Local list"}
      },
      w = {"<cmd>w<Enter>", "Write buffer"},
      q = {"<cmd>q<Enter>", "Quit"},
      c = {"<cmd>BufferClose<Enter>", "Close buffer"},
      -- g = {"<cmd>lua require('neogit').open({ kind='split' })<Enter>", "Neogit"},
      g = {"<cmd>Git<Enter>", "Fugitive"},
      e = {"<cmd>NvimTreeToggle<Enter>", "Toggle tree"}
    }
  }
)
