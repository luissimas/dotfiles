-- Only required if you have packer configured as `opt`
vim.cmd [[packadd packer.nvim]]

local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

return require('packer').startup(
  function(use)
    -- Packer auto-manager
    use {"wbthomason/packer.nvim", opt = true}

    -- LSP Config, autocompletion and Treesitter
    use {
      'neovim/nvim-lspconfig',
      'nvim-lua/completion-nvim', -- Autocompletion
      'onsails/lspkind-nvim', -- Lspking completion popup icons
      {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'} -- Treesitter
    }

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
    use {
      'nvim-telescope/telescope.nvim',
      requires = {
        'nvim-lua/popup.nvim',
        'nvim-lua/plenary.nvim',
        'nvim-telescope/telescope-media-files.nvim',
      }
    }

    -- Toggle terminal wrapper
    use "akinsho/nvim-toggleterm.lua"

    -- Autopairs
    use "windwp/nvim-autopairs"

    -- Autoclose tags
    use 'alvan/vim-closetag'

    -- Indent lines
    use {'lukas-reineke/indent-blankline.nvim',  branch = 'lua' }

    -- Comment lines
    use 'tpope/vim-commentary'

    -- Color highlight
    use 'norcalli/nvim-colorizer.lua'

    -- Git sings
    use 'lewis6991/gitsigns.nvim'

    -- Vimtex for latex
    use 'lervag/vimtex'

    -- Vim-pandoc for makdown compile and highlight
    use 'vim-pandoc/vim-pandoc'
    use 'vim-pandoc/vim-pandoc-syntax'

    -- Colorschemes
    use 'sainnhe/sonokai'
    use 'dylanaraps/wal.vim'
    use 'joshdick/onedark.vim'
    use 'arcticicestudio/nord-vim'
    use {'dracula/vim', as = 'dracula' }
    use {'pineapplegiant/spaceduck', branch = 'main' }
    use {"npxbr/gruvbox.nvim", requires = {"rktjmp/lush.nvim"}}
  end
)
