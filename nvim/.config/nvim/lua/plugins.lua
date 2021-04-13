local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim '..install_path)
  execute 'packadd packer.nvim'
end

return require('packer').startup(
  function(use)
    -- Packer auto-manager
    use {"wbthomason/packer.nvim", opt = true}

    -- LSP Config
    use 'neovim/nvim-lspconfig'

    -- Autocompletion
    use 'nvim-lua/completion-nvim'

    -- Lspking completion popup icons
    use 'onsails/lspkind-nvim'

    -- Treesitter
    use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}

    -- Prettier
    use {'prettier/vim-prettier', run = 'npm install' }

    -- Snippets
    use 'SirVer/ultisnips'

    -- File tree
    use 'kyazdani42/nvim-tree.lua'

    -- Lualine statusline
    use 'hoob3rt/lualine.nvim'

    -- Barbar tabline
    use 'romgrk/barbar.nvim'

    -- Icons for file tree and tabline
    use 'kyazdani42/nvim-web-devicons'

    -- Telescope
    use 'nvim-lua/popup.nvim'
    use 'nvim-lua/plenary.nvim'
    use 'nvim-telescope/telescope.nvim'

    -- Autopairs
    use 'jiangmiao/auto-pairs'

    -- Indent lines
    use {'lukas-reineke/indent-blankline.nvim',  branch = 'lua' }

    -- Comment lines
    use 'tpope/vim-commentary'

    -- Color highlight
    use 'norcalli/nvim-colorizer.lua'

    -- Git sings
    use 'lewis6991/gitsigns.nvim'

    -- Vimtex for latex
    -- Plug 'lervag/vimtex'

    -- Vim-pandoc for makdown compile and highlight
    -- Plug 'vim-pandoc/vim-pandoc'
    -- Plug 'vim-pandoc/vim-pandoc-syntax'

    -- Colorschemes
    use 'morhetz/gruvbox'
    use 'dylanaraps/wal.vim'
    use 'joshdick/onedark.vim'
    use 'arcticicestudio/nord-vim'
    use {'dracula/vim', as = 'dracula' }
    use {'pineapplegiant/spaceduck', branch = 'main' }
  end
)
