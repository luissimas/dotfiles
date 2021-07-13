-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath("data") .. "/site/pack/packer/opt/packer.nvim"

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({"git", "clone", "https://github.com/wbthomason/packer.nvim", install_path})
  execute "packadd packer.nvim"
end

return require("packer").startup(
  function(use)
    -- Misc
    use {"wbthomason/packer.nvim", opt = true} -- Packer auto-manager
    use "nvim-lua/popup.nvim" -- Ui API for plugins
    use "nvim-lua/plenary.nvim" -- Utility functions for various plugins

    --[[
      Editor
    --]]
    -- Lsp Config layer
    use {
      "neovim/nvim-lspconfig",
      config = function()
        require("plug.lspconfig")
      end
    }

    -- Treesitter
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = function()
        require("plug.treesitter")
      end,
      requires = {
        "nvim-treesitter/playground"
      }
    }

    -- Code format - TODO: Check Neoformat or EFM
    use {
      "mhartington/formatter.nvim",
      config = function()
        require("plug.formatter")
      end
    }

    -- Snippets engine
    use {
      "SirVer/ultisnips",
      config = function()
        require("plug.ultisnips")
      end
    }

    -- Snippets collection
    use {
      "honza/vim-snippets"
    }

    -- Function signature docs
    use {
      "ray-x/lsp_signature.nvim",
      config = function()
        require("plug.lsp_signature")
      end
    }

    -- File tree
    use {
      "kyazdani42/nvim-tree.lua",
      config = function()
        require("plug.tree")
      end
    }

    -- Better status line
    use {
      "hoob3rt/lualine.nvim",
      config = function()
        require("plug.lualine")
      end
    }

    -- Never forget your keymaps
    use {
      "folke/which-key.nvim",
      config = function()
        require("plug.whichkey")
      end
    }

    -- I like autopairs
    use {
      "steelsojka/pears.nvim",
      config = function()
        require("plug.pears")
      end
    }

    -- Better quickfix lists
    use {
      "folke/trouble.nvim",
      config = function()
        require("plug.trouble")
      end
    }

    -- Comment lines
    use {
      "tpope/vim-commentary"
    }

    -- Surround
    use {
      "tpope/vim-surround"
    }

    -- Change cwd based on the project's root directory
    use {
      "ahmedkhalf/lsp-rooter.nvim",
      config = function()
        require("plug.lsp_rooter")
      end
    }

    -- Orgmode inside neovim? wtf TODO: Check this
    -- use {
    --   "kristijanhusak/orgmode.nvim",
    --   config = function()
    --     require("orgmode").setup {}
    --   end
    -- }

    --[[
      Completion
    --]]
    -- Completion backend
    use {
      "hrsh7th/nvim-compe",
      config = function()
        require("plug.compe")
      end
    }

    -- The ultimate fuzzy finder
    use {
      "nvim-telescope/telescope.nvim",
      config = function()
        require("plug.telescope")
      end
    }

    -- Telescope extensions
    use {
      "nvim-telescope/telescope-project.nvim",
      "nvim-telescope/telescope-fzy-native.nvim",
      "luissimas/telescope-nodescripts.nvim"
    }

    --[[
      UI
    --]]
    -- Better lsp UI
    use {
      "glepnir/lspsaga.nvim",
      config = function()
        require("plug.lspsaga")
      end
    }

    -- General icons
    use {
      "kyazdani42/nvim-web-devicons",
      config = function()
        require("plug.webdevicons")
      end
    }

    -- Completion icons
    use {
      "onsails/lspkind-nvim",
      config = function()
        require("plug.lspkind")
      end
    }

    -- Colors in hex color codes
    use {
      "norcalli/nvim-colorizer.lua",
      config = function()
        require("plug.colorizer")
      end
    }

    -- Color brackets
    use {
      "p00f/nvim-ts-rainbow"
    }

    -- Highlight words under cursor
    use {
      "RRethy/vim-illuminate",
      config = function()
        require("plug.illuminate")
      end
    }

    -- Highlight comments
    use {
      "folke/todo-comments.nvim",
      config = function()
        require("plug.todocomments")
      end
    }

    -- Smooth scrol
    -- use {
    --   "karb94/neoscroll.nvim",
    --   config = function()
    --     require("plug.neoscroll")
    --   end
    -- }

    --[[
      Tools
    --]]
    -- The best git integration plugin
    use {
      "tpope/vim-fugitive"
    }

    -- The *other* best git integration plugin
    use {
      "TimUntersberger/neogit",
      config = function()
        require("plug.neogit")
      end
    }

    -- Better diff views
    use {
      "sindrets/diffview.nvim"
    }

    -- Git blame and signs
    use {
      "lewis6991/gitsigns.nvim",
      config = function()
        require("plug.gitsigns")
      end
    }

    -- Docs and completion for nvim lua API
    use {
      "folke/lua-dev.nvim"
    }

    -- Lua 5.1 reference manual
    use {
      "milisims/nvim-luaref"
    }

    -- Vimtex for latex
    use {
      "lervag/vimtex",
      opt = true,
      ft = "tex"
    }

    -- Startup time log
    use {
      "dstein64/vim-startuptime"
    }

    -- Navigate and resize tmux and vim splits
    use {
      "aserowy/tmux.nvim",
      config = function()
        require("plug.tmux")
      end
    }

    -- Playing with lua?
    use {
      "rafcamlet/nvim-luapad",
      opt = true,
      ft = "lua"
    }

    -- Save and restore sessions
    use {
      "rmagatti/auto-session",
      opt = true
    }

    --[[
      Colorschemes
    --]]
    use "dylanaraps/wal.vim"
    use "joshdick/onedark.vim"
    use "folke/lsp-colors.nvim" -- Adds LSP colors for themes that don't yet support them
    use "folke/tokyonight.nvim"
    use "shaunsingh/moonlight.nvim"
    use "yashguptaz/calvera-dark.nvim"

    -- Personal plugins
    use {
      "~/fun/lua/plugins/eval",
      config = function()
        require("eval").setup({})

        vim.api.nvim_set_keymap("v", "<leader>gr", ":Eval<Enter>", {noremap = true, silent = true})
      end
    }
  end
)
