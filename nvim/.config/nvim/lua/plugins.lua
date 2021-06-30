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

    -- Editor
    use "neovim/nvim-lspconfig" -- Lsp Config layer
    use {"nvim-treesitter/nvim-treesitter", run = ":TSUpdate"} -- Treesitter
    use "mhartington/formatter.nvim" -- Code format - TODO: Check Neoformat or EFM
    use "SirVer/ultisnips" -- Snippets engine
    use "ray-x/lsp_signature.nvim" -- Function signature docs
    use "honza/vim-snippets" -- Snippets collection
    use "kyazdani42/nvim-tree.lua" -- File tree
    use "hoob3rt/lualine.nvim" -- Better status line
    use "folke/which-key.nvim" -- Never forget your keymaps
    use "steelsojka/pears.nvim" -- I like autopairs
    use "folke/trouble.nvim" -- Amazing quickfix lists
    use "tpope/vim-commentary" -- Comment lines
    use "tpope/vim-surround" -- Surround
    -- use "ahmedkhalf/lsp-rooter.nvim" -- Change cwd based on the project's root directory

    -- Completion
    use "hrsh7th/nvim-compe" -- Completion backend
    use "ray-x/lsp_signature.nvim" -- Function signature docs
    use "nvim-telescope/telescope.nvim" -- The ultimate fuzzy finder
    use "nvim-telescope/telescope-fzy-native.nvim" -- Fzy extension for telescope

    -- UI
    use "glepnir/lspsaga.nvim" -- Better lsp UI
    use "kyazdani42/nvim-web-devicons" -- General icons
    use "onsails/lspkind-nvim" -- Completion icons
    use "norcalli/nvim-colorizer.lua" -- Colors in hex color codes
    use "p00f/nvim-ts-rainbow" -- Color brackets
    use "RRethy/vim-illuminate" -- Highlight words under cursor
    use "folke/todo-comments.nvim" -- Highlight comments

    -- Tools
    use "tpope/vim-fugitive" -- The best git integration plugin
    use "TimUntersberger/neogit" -- The *other* best git integration plugin
    use "lewis6991/gitsigns.nvim"
    use "folke/lua-dev.nvim" -- Docs and completion for nvim lua API
    use "milisims/nvim-luaref" -- Lua 5.1 reference manual
    use {"lervag/vimtex", opt = true, ft = "tex"} -- Vimtex for latex
    use "dstein64/vim-startuptime" -- Startup time log
    use "aserowy/tmux.nvim" -- Navigate and resize tmux and vim splits
    use "rmagatti/auto-session" -- Save and restore sessions

    -- Colorschemes
    -- use "folke/lsp-colors.nvim" -- Adds LSP colors for themes that don't yet support them
    use "dylanaraps/wal.vim"
    use "folke/tokyonight.nvim"
  end
)
