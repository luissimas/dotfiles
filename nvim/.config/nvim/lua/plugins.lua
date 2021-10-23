-- Only required if you have packer configured as `opt`
vim.cmd([[packadd packer.nvim]])

local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
  execute("packadd packer.nvim")
end

return require("packer").startup(function(use)
  -- Packer auto-manager
  use({ "wbthomason/packer.nvim", opt = true })

  -- Ui API for plugins
  --  use("nvim-lua/popup.nvim")

  -- Utility functions for various plugins

  -- Lsp Config layer
  use({
    "neovim/nvim-lspconfig",
    config = function()
      require("plug.lspconfig")
    end,
  })

  -- Treesitter
  use({
    "nvim-treesitter/nvim-treesitter",
    run = ":TSUpdate",
    config = function()
      require("plug.treesitter")
    end,
    requires = {
      {
        "nvim-treesitter/playground",
        after = "nvim-treesitter",
      },
      {
        "p00f/nvim-ts-rainbow",
        after = "nvim-treesitter",
      },
    },
  })

  -- Completion
  use({
    "hrsh7th/nvim-cmp",
    config = function()
      require("plug.cmp")
    end,
    requires = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-calc",
      "f3fora/cmp-spell",
      "quangnguyen30192/cmp-nvim-ultisnips",
    },
  })

  -- The ultimate fuzzy finder
  use({
    "nvim-telescope/telescope.nvim",
    config = function()
      require("plug.telescope")
    end,
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzy-native.nvim",
      "luissimas/telescope-nodescripts.nvim",
    },
  })

  -- Code format
  use({
    "mhartington/formatter.nvim",
    config = function()
      require("plug.formatter")
    end,
  })

  -- Snippets
  use({
    "SirVer/ultisnips",
    event = "InsertEnter",
    config = function()
      require("plug.ultisnips")
    end,
    requires = {
      {
        -- Snippets collection
        "honza/vim-snippets",
        after = "ultisnips",
      },
    },
  })

  -- Function signature
  use({
    "ray-x/lsp_signature.nvim",
    event = "InsertCharPre",
    config = function()
      require("plug.lsp_signature")
    end,
  })

  -- Type signature as virtual text
  use({
    "jubnzv/virtual-types.nvim",
    config = function()
      vim.cmd("autocmd FileType ocaml hi! link TypeAnnot Comment")
    end,
  })

  -- File tree
  use({
    "kyazdani42/nvim-tree.lua",
    cmd = "NvimTreeToggle",
    config = function()
      require("plug.tree")
    end,
  })

  -- Better status line
  use({
    "hoob3rt/lualine.nvim",
    event = "BufRead",
    config = function()
      require("plug.lualine")
    end,
  })

  -- Never forget your keymaps
  use({
    "folke/which-key.nvim",
    config = function()
      require("plug.whichkey")
    end,
  })

  -- Navigate and resize tmux and vim splits
  use({
    "aserowy/tmux.nvim",
    config = function()
      require("plug.tmux")
    end,
  })

  -- Better quickfix lists
  use({
    "folke/trouble.nvim",
    config = function()
      require("plug.trouble")
    end,
  })

  -- Comment lines
  use({
    "b3nj5m1n/kommentary",
    config = function()
      require("plug.kommentary")
    end,
  })

  -- Autopairs
  use({
    "steelsojka/pears.nvim",
    config = function()
      require("plug.pears")
    end,
  })

  -- Surround
  use({
    "tpope/vim-surround",
    event = "BufRead",
  })

  -- Project management
  use({
    "ahmedkhalf/project.nvim",
    after = "nvim-lspconfig",
    config = function()
      require("plug.project")
    end,
  })

  -- Orgmode inside neovim?
  use({
    "kristijanhusak/orgmode.nvim",
    config = function()
      require("orgmode").setup({})
    end,
    requires = {
      "akinsho/org-bullets.nvim",
      config = function()
        require("org-bullets").setup({
          symbols = { "◉", "○", "✸", "✿" },
        })
      end,
    },
  })

  -- General icons
  use({
    "kyazdani42/nvim-web-devicons",
    config = function()
      require("plug.webdevicons")
    end,
  })

  -- Completion icons
  use({
    "onsails/lspkind-nvim",
    config = function()
      require("plug.lspkind")
    end,
  })

  -- Colors in hex color codes
  use({
    "norcalli/nvim-colorizer.lua",
    config = function()
      require("plug.colorizer")
    end,
  })

  -- Highlight comments
  use({
    "folke/todo-comments.nvim",
    after = "nvim-treesitter",
    config = function()
      require("plug.todocomments")
    end,
  })

  -- The best git integration plugin
  use({
    "tpope/vim-fugitive",
    cmd = "Git",
  })

  -- The *other* best git integration plugin
  use({
    "TimUntersberger/neogit",
    cmd = "Neogit",
    config = function()
      require("plug.neogit")
    end,
    requires = {
      "sindrets/diffview.nvim",
      after = "neogit",
    },
  })

  -- Git blame and signs
  -- use({
  --   "lewis6991/gitsigns.nvim",
  --   event = "BufRead",
  --   config = function()
  --     require("plug.gitsigns")
  --   end,
  -- })

  -- Docs and completion for nvim lua API
  use({
    "folke/lua-dev.nvim",
  })

  -- Spellcheck with treesitter
  use({
    "lewis6991/spellsitter.nvim",
    config = function()
      require("spellsitter").setup({
        hl = "SpellBad",
        captures = { "comment" }, -- set to {} to spellcheck everything
      })
    end,
  })

  -- Startup time log
  use({ "tweekmonster/startuptime.vim", cmd = "StartupTime" })

  -- Colorschemes
  use("folke/lsp-colors.nvim") -- Adds LSP colors for themes that don't yet support them
  use("folke/tokyonight.nvim")
  use("ishan9299/modus-theme-vim")
  use("shaunsingh/nord.nvim")
  use("projekt0n/github-nvim-theme")
  use({
    "mcchrish/zenbones.nvim",
    requires = "rktjmp/lush.nvim",
  })
  use("RRethy/nvim-base16")
  use({ "rose-pine/neovim", as = "rose-pine" })

  -- Personal plugins
  use({
    "~/fun/lua/plugins/eval",
    cmd = "Eval",
    config = function()
      require("eval").setup({})

      vim.api.nvim_set_keymap("v", "<leader>gr", ":Eval<Enter>", { noremap = true, silent = true })
    end,
  })
end)
