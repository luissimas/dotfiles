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

    -- Lsp Config layer
    use {
      "neovim/nvim-lspconfig",
      config = function()
        require("plug.lspconfig")
      end,
      requires = {
        -- Docs and signatures for nvim lua API
        {"folke/lua-dev.nvim"}
      }
    }

    -- Better LSP experience
    use {
      {
        "glepnir/lspsaga.nvim",
        config = function()
          require("plug.lspsaga")
        end
      },
      {
        "ray-x/lsp_signature.nvim",
        config = function()
          require("plug.lsp_signature")
        end
      }
    }

    -- Treesitter
    use {
      {
        "nvim-treesitter/nvim-treesitter",
        config = function()
          require("plug.treesitter")
        end,
        run = ":TSUpdate"
      },
      -- Color pairs
      {"p00f/nvim-ts-rainbow"}
    }

    -- Autocompletion
    use {
      "hrsh7th/nvim-compe",
      config = function()
        require("plug.compe")
      end,
      -- Lspkind completion popup icons
      requires = {
        {
          "onsails/lspkind-nvim",
          config = function()
            require("plug.lspkind")
          end
        }
      }
    }

    -- Code format
    use {
      "mhartington/formatter.nvim",
      config = function()
        require("plug.formatter")
      end
    }

    -- Snippets
    use {
      "hrsh7th/vim-vsnip",
      config = function()
        require("plug.vsnip")
      end,
      requires = {
        {"rafamadriz/friendly-snippets"}
      }
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
      -- "kyazdani42/nvim-tree.lua",
      "~/repos/nvim-tree.lua",
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

    -- Icons
    use {
      "kyazdani42/nvim-web-devicons",
      config = function()
        require("plug.webdevicons")
      end
    }

    -- Git integration
    use {
      --[[
        I'm hopping between Fugitive and Neogit these days, Neogit has a better interface
        and the diffview is very nice, but it doesn't work very well with symlinks
      --]]
      -- {"tpope/vim-fugitive"},
      {
        "TimUntersberger/neogit",
        config = function()
          require("plug.neogit")
        end,
        requires = {
          "nvim-lua/plenary.nvim",
          "sindrets/diffview.nvim"
        }
      },
      {
        "lewis6991/gitsigns.nvim",
        config = function()
          require("plug.gitsigns")
        end
      }
    }

    -- Trouble for pretty quickfix lists
    use {
      "folke/trouble.nvim",
      config = function()
        require("plug.trouble")
      end
    }

    -- Which-key
    use {
      "folke/which-key.nvim",
      config = function()
        require("plug.whichkey")
      end
    }

    -- Autopairs
    use {
      "steelsojka/pears.nvim",
      config = function()
        require("plug.pears")
      end
    }

    -- Smooth scrolling
    use {
      "karb94/neoscroll.nvim",
      config = function()
        require("plug.neoscroll")
      end
    }

    -- Dashboard startpage
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

    -- Highlight words under cursor
    use {
      "RRethy/vim-illuminate",
      config = function()
        require("plug.illuminate")
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

    -- Toggle terminal
    use {
      "akinsho/nvim-toggleterm.lua",
      config = function()
        require("plug.toggleterm")
      end
    }

    -- Todo comments highlight
    use {
      "folke/todo-comments.nvim",
      config = function()
        require("plug.todocomments")
      end
    }

    -- Comment lines
    use "tpope/vim-commentary"

    -- Surroundings
    use "tpope/vim-surround"

    -- Reload utility
    use "famiu/nvim-reload"

    -- Codi scratchpad
    use {"metakirby5/codi.vim", opt = true, ft = "javascript"}

    -- Luapad
    use {"rafcamlet/nvim-luapad"}

    -- ZenMode
    use {
      "folke/zen-mode.nvim",
      config = function()
        require("plug.zenmode")
      end
    }

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
    -- use {
    --   "vim-pandoc/vim-pandoc",
    --   config = function()
    --     require("plug.pandoc")
    --   end,
    --   opt = true,
    --   ft = "markdown",
    --   requires = {
    --     "vim-pandoc/vim-pandoc-syntax"
    --   }
    -- }

    -- Startup time log
    use {"dstein64/vim-startuptime"}

    -- Colorschemes
    -- use {"folke/lsp-colors.nvim"} -- Adds LSP colors for themes that don't yet support them
    use {"dylanaraps/wal.vim"}
    use {"shaunsingh/nord.nvim"}
    use {"folke/tokyonight.nvim"}
    use {"shaunsingh/moonlight.nvim"}
    use {"marko-cerovac/material.nvim"}

    -- My custom plugins (testing stuff)
    use "~/fun/lua/plugins/pandoc"
    use "~/fun/lua/plugins/pomodoro"
  end
)
