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

    -- LSP and autocomplete
    use {
      "hrsh7th/nvim-compe", -- Autocompletion
      "neovim/nvim-lspconfig", -- Lsp Config layer
      "glepnir/lspsaga.nvim", -- Code actions, better diagnostics and a bunch of stuff
      "onsails/lspkind-nvim", -- Lspkind completion popup icons
      "mhartington/formatter.nvim", -- Code format
      {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"} -- Treesitter
    }

    -- Snippets
    use "SirVer/ultisnips"

    -- VimWiki
    use "vimwiki/vimwiki"

    -- Git integration
    use {
      -- "tpope/vim-fugitive",
      "TimUntersberger/neogit",
      requires = {"nvim-lua/plenary.nvim"},
      "lewis6991/gitsigns.nvim"
    }

    -- Telescope
    use {
      "nvim-telescope/telescope.nvim",
      "nvim-lua/popup.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzy-native.nvim"
    }

    -- Bars and tree
    use {
      "kyazdani42/nvim-tree.lua", -- File tree
      "hoob3rt/lualine.nvim", -- Lualine statusline
      "romgrk/barbar.nvim", -- Barbar tabline
      "kyazdani42/nvim-web-devicons" -- Icons for file tree and tabline
    }

    -- Database management
    use {
      "tpope/vim-dadbod",
      "kristijanhusak/vim-dadbod-ui",
      "kristijanhusak/vim-dadbod-completion"
    }

    -- Autopairs and tags
    use {
      "windwp/nvim-autopairs",
      "alvan/vim-closetag"
    }

    -- Pure visual stuff
    use {
      "p00f/nvim-ts-rainbow", -- Color pairs
      "karb94/neoscroll.nvim", -- Smooth scrolling
      "glepnir/dashboard-nvim", -- Dashboard
      "norcalli/nvim-colorizer.lua", -- Color highlight
      {"lukas-reineke/indent-blankline.nvim", branch = "lua"} -- Indent lines
    }

    -- Toggle terminal wrapper
    use "akinsho/nvim-toggleterm.lua"

    -- Comment lines
    use "tpope/vim-commentary"

    -- Surroundings
    use "tpope/vim-surround"

    -- Lua pad
    use "rafcamlet/nvim-luapad"

    -- Vimtex for latex
    use "lervag/vimtex"

    -- Markdown
    use {
      {
        "iamcco/markdown-preview.nvim",
        run = function()
          vim.fn["mkdp#util#install"]()
        end,
        ft = {"markdown", "vim-plug"}
      },
      "vim-pandoc/vim-pandoc",
      "vim-pandoc/vim-pandoc-syntax"
    }

    -- Colorschemes
    use {
      "morhetz/gruvbox",
      "sainnhe/sonokai",
      "dylanaraps/wal.vim",
      "joshdick/onedark.vim",
      "folke/lsp-colors.nvim", -- Adds LSP colors for themes that don't yet support them
      "arcticicestudio/nord-vim",
      "drewtempelmeyer/palenight.vim",
      {"dracula/vim", as = "dracula"},
      {"pineapplegiant/spaceduck", branch = "main"}
    }

    -- My custom plugins (testing stuff)
    use "~/fun/lua/plugins/pomodoro"
  end
)
