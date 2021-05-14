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
    -- Packer auto-manager
    use {"wbthomason/packer.nvim", opt = true}

    -- Autocompletion
    use {
      "hrsh7th/nvim-compe",
      config = function()
        require("plug.compe")
      end
    }

    -- Lsp Config layer
    use {
      "neovim/nvim-lspconfig",
      config = function()
        require("plug.lspconfig")
      end
    }

    -- Code actions, better diagnostics and a bunch of stuff
    use {
      "glepnir/lspsaga.nvim",
      config = function()
        require("plug.lspsaga")
      end
    }

    -- Lspkind completion popup icons
    use {"onsails/lspkind-nvim"}

    -- Code format
    use {
      "mhartington/formatter.nvim",
      config = function()
        require("plug.formatter")
      end
    }

    -- Treesitter
    use {
      "nvim-treesitter/nvim-treesitter",
      config = function()
        require("plug.treesitter")
      end,
      run = ":TSUpdate"
    }

    -- Snippets
    use {
      "SirVer/ultisnips",
      config = function()
        require("plug.ultisnips")
      end
    }

    -- Fugitive
    use {"tpope/vim-fugitive"}

    -- Git blame and signs
    use {
      "lewis6991/gitsigns.nvim",
      config = function()
        require("plug.gitsigns")
      end
    }

    -- Telescope
    use {
      "nvim-telescope/telescope.nvim",
      config = function()
        require("plug.telescope")
      end,
      requires = {
        {"nvim-lua/popup.nvim"},
        {"nvim-lua/plenary.nvim"},
        {"nvim-telescope/telescope-fzy-native.nvim"}
      }
    }

    -- File tree
    use {
      "kyazdani42/nvim-tree.lua",
      config = function()
        require("plug.tree")
      end
    }

    -- Lualine statusline
    use {
      "hoob3rt/lualine.nvim",
      config = function()
        require("plug.lualine")
      end
    }

    -- Barbar tabline
    use {
      "romgrk/barbar.nvim",
      config = function()
        require("plug.barbar")
      end
    }

    -- Icons for file tree and tabline
    use {
      "kyazdani42/nvim-web-devicons",
      config = function()
        require("plug.icons")
      end
    }

    -- Database management
    use {
      "tpope/vim-dadbod",
      config = function()
        require("plug.dadbod")
      end,
      requires = {
        "kristijanhusak/vim-dadbod-ui",
        "kristijanhusak/vim-dadbod-completion"
      },
      opt = true,
      ft = "sql"
    }

    -- Autopairs
    use {
      "windwp/nvim-autopairs",
      config = function()
        require("plug.autopairs")
      end
    }

    -- Autoclose tags
    use {
      "alvan/vim-closetag",
      config = function()
        require("plug.closetag")
      end
    }

    -- Color pairs
    use {"p00f/nvim-ts-rainbow"}

    -- Smooth scrolling
    use {
      "karb94/neoscroll.nvim",
      config = function()
        require("plug.neoscroll")
      end
    }

    -- Dashboard
    use {
      "glepnir/dashboard-nvim",
      config = function()
        require("plug.dashboard")
      end
    }

    -- Color highlight
    use {
      "norcalli/nvim-colorizer.lua",
      config = function()
        require("plug.colorizer")
      end
    }

    -- Indent lines
    use {
      "lukas-reineke/indent-blankline.nvim",
      config = function()
        require("plug.indent")
      end,
      branch = "lua"
    }

    -- Toggle terminal wrapper
    use {
      "akinsho/nvim-toggleterm.lua",
      config = function()
        require("plug.toggleterm")
      end
    }

    -- REST client
    use {
      "NTBBloodbath/rest.nvim",
      requires = {"nvim-lua/plenary.nvim"},
      opt = true,
      ft = "http"
    }

    -- Comment lines
    use "tpope/vim-commentary"

    -- Todo comments highlight
    use {
      "folke/todo-comments.nvim",
      config = function()
        require("plug.todocomments")
      end
    }

    -- Surroundings
    use "tpope/vim-surround"

    -- Codi
    use {"metakirby5/codi.vim", opt = true, ft = "javascript"}

    -- Vimtex for latex
    use {"lervag/vimtex", opt = true, ft = "tex"}

    -- Markdown preview
    use {
      "iamcco/markdown-preview.nvim",
      run = function()
        vim.fn["mkdp#util#install"]()
      end,
      opt = true,
      ft = {"markdown", "vim-plug", "pandoc"},
      config = function()
        require("plug.markdownpreview")
      end
    }

    -- Markdown compile
    use {
      "vim-pandoc/vim-pandoc",
      config = function()
        require("plug.pandoc")
      end,
      opt = true,
      ft = "markdown"
      -- requires = {
      --   "vim-pandoc/vim-pandoc-syntax"
      -- }
    }

    -- Colorschemes
    -- use {"morhetz/gruvbox"}
    use {"dylanaraps/wal.vim"}
    -- use {"joshdick/onedark.vim"}
    use {"folke/tokyonight.nvim"}
    use {"folke/lsp-colors.nvim"} -- Adds LSP colors for themes that don't yet support them
    -- use {"arcticicestudio/nord-vim"}
    use {"marko-cerovac/material.nvim"}

    -- My custom plugins (testing stuff)
    -- use "~/fun/lua/plugins/pomodoro"
  end
)
