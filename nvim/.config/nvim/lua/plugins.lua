-- Packer bootstrap
local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  Bootstraped = vim.fn.system { "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path }
end

-- Compiling packer after saving file
vim.api.nvim_exec(
  [[
  augroup Packer
    autocmd!
    autocmd BufWritePost plugins.lua PackerCompile
  augroup end
]],
  false
)

local function cfg(plugin)
  return "require('config." .. plugin .. "')"
end

-- Plugins
require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim"
    use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate", config = cfg "treesitter" }
    use "p00f/nvim-ts-rainbow"
    use { "neovim/nvim-lspconfig", config = cfg "lsp" }
    use "jose-elias-alvarez/null-ls.nvim"
    use "nvim-lua/plenary.nvim"
    use { "hrsh7th/nvim-cmp", config = cfg "cmp" }
    use "hrsh7th/cmp-path"
    use "hrsh7th/cmp-nvim-lsp"
    use "hrsh7th/cmp-nvim-lua"
    use "kdheepak/cmp-latex-symbols"
    use "quangnguyen30192/cmp-nvim-ultisnips"
    use { "SirVer/UltiSnips", config = cfg "ultisnips" }
    use "honza/vim-snippets"
    use { "nvim-telescope/telescope.nvim", config = cfg "telescope" }
    use { "nvim-telescope/telescope-fzf-native.nvim", run = "make" }
    use { "aserowy/tmux.nvim", config = cfg "tmux" }
    use "RRethy/nvim-base16"
    use "onsails/lspkind-nvim"
    use { "lewis6991/gitsigns.nvim", config = cfg "gitsigns" }
    use { "TimUntersberger/neogit", config = cfg "neogit" }
    use { "windwp/nvim-autopairs", config = cfg "autopairs" }
    use { "b3nj5m1n/kommentary", config = cfg "comment" }
    use { "ahmedkhalf/project.nvim", config = cfg "project" }

    -- Automatically set up packer after cloning it
    if Bootstraped then
      require("packer").sync()
    end
  end,
  config = {
    display = {
      open_fn = function()
        return require("packer.util").float { border = "single" }
      end,
    },
  },
}
