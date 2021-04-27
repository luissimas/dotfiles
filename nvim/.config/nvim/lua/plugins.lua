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

    -- LSP Config, autocompletion and Treesitter
    use {
      "neovim/nvim-lspconfig",
      "hrsh7th/nvim-compe", -- Autocompletion
      "glepnir/lspsaga.nvim", -- Code actions, better diagnostics and a bunch of stuff
      "onsails/lspkind-nvim", -- Lspkind completion popup icons
      "mhartington/formatter.nvim", -- Code format
      {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"} -- Treesitter
    }

    -- Git integration
    -- use 'tpope/vim-fugitive'
    use {"TimUntersberger/neogit", requires = "nvim-lua/plenary.nvim"}
    use "lewis6991/gitsigns.nvim"

    -- Snippets
    use "SirVer/ultisnips"

    -- File tree
    use "kyazdani42/nvim-tree.lua"

    -- Lualine statusline
    use "hoob3rt/lualine.nvim"

    -- Barbar tabline
    use "romgrk/barbar.nvim"

    -- Icons for file tree and tabline
    use "kyazdani42/nvim-web-devicons"

    -- Telescope
    use {
      "nvim-telescope/telescope.nvim",
      requires = {
        "nvim-lua/popup.nvim",
        "nvim-lua/plenary.nvim"
      }
    }

    -- Database management
    use {
      "tpope/vim-dadbod",
      "kristijanhusak/vim-dadbod-ui",
      "kristijanhusak/vim-dadbod-completion"
    }

    -- Lua pad
    use "rafcamlet/nvim-luapad"

    -- Toggle terminal wrapper
    use "akinsho/nvim-toggleterm.lua"

    -- Autopairs
    use "windwp/nvim-autopairs"

    -- Color pairs
    use "p00f/nvim-ts-rainbow"

    -- Autoclose tags
    use "alvan/vim-closetag"

    -- Indent lines
    use {"lukas-reineke/indent-blankline.nvim", branch = "lua"}

    -- Comment lines
    use "tpope/vim-commentary"

    -- Color highlight
    use "norcalli/nvim-colorizer.lua"

    -- Smooth scrolling
    use "karb94/neoscroll.nvim"

    -- Dashboard
    use "glepnir/dashboard-nvim"

    -- Vimtex for latex
    use "lervag/vimtex"

    -- Vim-pandoc for makdown compile and highlight
    use {
      "vim-pandoc/vim-pandoc",
      "vim-pandoc/vim-pandoc-syntax"
    }

    -- Colorschemes
    use {
      "morhetz/gruvbox",
      "sainnhe/sonokai",
      "dylanaraps/wal.vim",
      "joshdick/onedark.vim",
      "arcticicestudio/nord-vim",
      "drewtempelmeyer/palenight.vim",
      {"dracula/vim", as = "dracula"},
      {"pineapplegiant/spaceduck", branch = "main"}
    }

    -- My custom plugins (testing stuff)
    use "~/fun/lua/plugins/pomodoro"
  end
)
