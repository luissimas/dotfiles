-- Leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Trim whitespace on save
vim.api.nvim_exec(
  [[
  fun! TrimWhitespace()
    let l:save = winsaveview()
    keeppatterns %s/\s\+$//e
    call winrestview(l:save)
  endfun

  augroup TrimWhitespace
    autocmd!
    autocmd BufWritePre * call TrimWhitespace()
  augroup end
]],
  false
)

-- Highlight on yank
vim.api.nvim_exec(
  [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank({ timeout = 100 })
  augroup end
]],
  false
)

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
    autocmd BufWritePost init.lua PackerCompile
  augroup end
]],
  false
)

-- Plugins
require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim"
    use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" }
    use "neovim/nvim-lspconfig"
    use "nvim-lua/plenary.nvim"
    use "jose-elias-alvarez/null-ls.nvim"
    use "hrsh7th/nvim-cmp"
    use "hrsh7th/cmp-path"
    use "hrsh7th/cmp-nvim-lsp"
    use "hrsh7th/cmp-nvim-lua"
    use "kdheepak/cmp-latex-symbols"
    use "quangnguyen30192/cmp-nvim-ultisnips"
    use "SirVer/UltiSnips"
    use "honza/vim-snippets"
    use "nvim-telescope/telescope.nvim"
    use "aserowy/tmux.nvim"
    use "RRethy/nvim-base16"
    use "onsails/lspkind-nvim"
    use "lewis6991/gitsigns.nvim"
    use "TimUntersberger/neogit"
    use "windwp/nvim-autopairs"
    use "b3nj5m1n/kommentary"

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

-- Save and quit
vim.api.nvim_set_keymap("n", "<leader>w", ":w<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>q", ":q<Enter>", { noremap = true, silent = true })

-- Kill buffers without messing up the split
vim.api.nvim_set_keymap("n", "<leader>bk", ":bp <bar> vs <bar> bn <bar> bd<Enter>", { noremap = true, silent = true })

-- Splits
vim.api.nvim_set_keymap("n", "<C-s>", ":vsplit<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<C-x>", ":split<Enter>", { noremap = true, silent = true })

-- Movement with line wraps
vim.api.nvim_set_keymap("n", "k", "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("v", "k", "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("n", "j", "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap("v", "j", "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- Move to begin/end of line with L and H
vim.api.nvim_set_keymap("n", "<S-l>", "$", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<S-h>", "0", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-l>", "$", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-h>", "0", { noremap = true, silent = true })

-- Tmux navigation bindings
require("tmux").setup {
  navigation = {
    -- cycles to opposite pane while navigating into the border
    cycle_navigation = false,
    -- enables default keybindings (C-hjkl) for normal mode
    enable_default_keybindings = true,
    -- prevents unzoom tmux when navigating beyond vim border
    persist_zoom = true,
  },
  resize = {
    -- enables default keybindings (A-hjkl) for normal mode
    enable_default_keybindings = true,
    -- sets resize steps for x axis
    resize_step_x = 5,
    -- sets resize steps for y axis
    resize_step_y = 5,
  },
}

vim.cmd "colorscheme base16-gruvbox-dark-hard"

-- Setting highlights
vim.cmd "hi VertSplit guifg=bg"
vim.cmd "hi NonText guifg=bg"

-- Treesitter
require("nvim-treesitter.configs").setup {
  ensure_installed = "all",

  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
}

-- UltiSnips
vim.g.UltiSniptsEditSplit = "normal"
vim.g.UltiSnipsSnippetDirectories = { "ultisnips" }
vim.g.UltiSnipsExpandTrigger = "<C-e>"
vim.g.UltiSnipsJumpForwardTrigger = "<Tab>"
vim.g.UltiSnipsJumpBackwardTrigger = "<S-tab>"

-- Cmp
local cmp = require "cmp"

cmp.setup {
  completion = {
    keyword_length = 3,
  },
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  mapping = {
    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
    ["<C-d>"] = cmp.mapping.scroll_docs(4),
    ["<C-j>"] = cmp.mapping.select_next_item { behavior = cmp.SelectBehavior.Select },
    ["<C-k>"] = cmp.mapping.select_prev_item { behavior = cmp.SelectBehavior.Select },
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm { select = true },
  },
  sources = {
    { name = "nvim_lua" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "latex_symbols" },
    { name = "ultisnips" },
  },
  documentation = {
    border = "single",
  },
  formatting = {
    format = require("lspkind").cmp_format {
      with_text = false,
      menu = {
        nvim_lua = "[Lua]",
        nvim_lsp = "[LSP]",
        path = "[Path]",
        latex_symbols = "[Latex]",
        ultisnips = "[UltiSnips]",
      },
    },
  },
  experimental = {
    native_menu = false,
    ghost_text = true,
  },
}

-- Telescope
require("telescope").setup {
  defaults = {
    prompt_prefix = " ",
    selection_caret = " ",
    entry_prefix = " ",
    sorting_strategy = "descending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        preview_width = 0.6,
      },
    },
    mappings = {
      i = {
        ["<C-s>"] = "file_vsplit",
        ["<C-x>"] = "file_split",
        ["<C-k>"] = "move_selection_previous",
        ["<C-j>"] = "move_selection_next",
      },
      n = {
        ["<C-s>"] = "file_vsplit",
        ["<C-x>"] = "file_split",
        ["<leader>q"] = "close",
      },
    },
    file_ignore_patterns = { "node_modules", "_build", ".elixir_ls", "%.png", "%.jpg", "%.jpeg", "%.pdf" },
  },
}

vim.api.nvim_set_keymap("n", "<leader>ff", ":Telescope find_files<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fg", ":Telescope live_grep<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fw", ":Telescope grep_string<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fb", ":Telescope buffers<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fh", ":Telescope help_tags<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<leader>fm", ":Telescope man_pages<Enter>", { noremap = true, silent = true })

-- Gitsigns
require("gitsigns").setup {
  keymaps = {
    ["n <leader>gb"] = '<cmd>lua require"gitsigns".blame_line(true)<CR>',
  },
}

-- Neogit
require("neogit").setup {
  signs = {
    section = { "○", "●" },
    item = { "○", "●" },
    hunk = { "", "" },
  },
}

vim.api.nvim_set_keymap("n", "<leader>gs", ":Neogit <Enter>", { noremap = true, silent = true })

-- Autopairs
require("nvim-autopairs").setup()

require("nvim-autopairs.completion.cmp").setup {
  map_cr = true, --  map <CR> on insert mode
  map_complete = false, -- it will auto insert `(` (map_char) after select function or method item
  auto_select = false, -- automatically select the first item
  insert = false, -- use insert confirm behavior instead of replace
  map_char = { -- modifies the function or method delimiter by filetypes
    all = "(",
    tex = "{",
  },
}

-- Comments
require("kommentary.config").configure_language("default", {
  prefer_single_line_comments = true,
})
