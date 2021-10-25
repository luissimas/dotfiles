-- Tabs
vim.o.tabstop = 2
vim.o.softtabstop = 2
vim.o.shiftwidth = 2
vim.o.expandtab = true
vim.o.smartindent = true

-- Number column
vim.o.relativenumber = true
vim.o.number = true
vim.o.numberwidth = 1

-- Search
vim.o.hlsearch = false
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true

-- Recover files
vim.o.backup = false
vim.o.swapfile = false
vim.o.undofile = true

-- Split orientation
vim.o.splitbelow = true
vim.o.splitright = true

-- Misc
vim.o.termguicolors = true -- enable true color support
vim.o.wrap = false -- no line wrap
vim.o.foldenable = false -- disable folding
vim.o.hidden = true -- buffers can be kept open in the background
vim.o.inccommand = "nosplit" -- visual incremental feedback for commands
vim.o.scrolloff = 8 -- scroll offset
vim.o.showmode = false -- don't show current mode in command area
vim.o.showtabline = 0 -- never show tablines
vim.o.signcolumn = "yes" -- set signcolumn display
vim.o.completeopt = "menuone,noinsert" -- completion options
-- vim.o.colorcolumn = "80" -- column for visual indent guideline
vim.o.updatetime = 200 -- time for CursorHold event
vim.o.clipboard = "unnamedplus" -- setting clipboard to system's
vim.opt.shortmess = vim.opt.shortmess + "c" -- disable completion item messages
vim.g.mapleader = " " -- leader key

-- Statusline
function _G.gitbranch()
  local branch = vim.fn.system(
    "git -C "
    .. vim.fn.expand('%:h') ..
    " branch --show-current 2>/dev/null"
  ):gsub("\n", "")

  if branch ~= "" then
    return string.format("[%s] ", branch)
  end

  return ""
end

vim.opt.statusline = " %{v:lua.gitbranch()}%f %m %r %= %y %p%% "

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
false)

-- Packer bootstrap
local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  Bootstraped = vim.fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
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
require("packer").startup({function(use)
  use "wbthomason/packer.nvim" -- Package manager
  use "nvim-treesitter/nvim-treesitter" -- Treesitter
  use "neovim/nvim-lspconfig" -- LSP client configuration
  use "hrsh7th/nvim-cmp" -- Autocompletion
  use "hrsh7th/cmp-nvim-lsp"
  use "hrsh7th/cmp-nvim-lua"
  use "hrsh7th/cmp-path"
  use "RRethy/nvim-base16" -- Colorschemes
  use "aserowy/tmux.nvim" -- Tmux navigation
  use "SirVer/UltiSnips" -- Snippets engine
  use "honza/vim-snippets" -- Snippets collection

  -- Automatically set up packer after cloning it
  if Bootstraped then
    require('packer').sync()
  end
end,
config = {
  display = {
    open_fn = function()
      return require("packer.util").float({ border = "single" })
    end
  }
}})

-- Save and quit
vim.api.nvim_set_keymap("n", "<leader>w", ":w<Enter>", { noremap=true, silent=true })
vim.api.nvim_set_keymap("n", "<leader>q", ":q<Enter>", { noremap=true, silent=true })

-- Splits
vim.api.nvim_set_keymap("n", "<C-s>", ":vsplit<Enter>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<C-x>", ":split<Enter>", { noremap = true, silent = true })

-- Movement with line wraps
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('v', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('v', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- Move to begin/end of line with L and H
vim.api.nvim_set_keymap("n", "<S-l>", "$", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<S-h>", "0", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-l>", "$", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-h>", "0", { noremap = true, silent = true })

-- Tmux navigation bindings
require("tmux").setup({
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
})

vim.cmd "colorscheme base16-gruvbox-dark-hard"

-- Setting highlights
vim.cmd "hi VertSplit guifg=bg"
vim.cmd "hi NonText guifg=bg"

-- Treesitter
require("nvim-treesitter.configs").setup({
  ensure_installed = "all",

  highlight = {
    enable = true,
  },
  indent = {
    enable = true
  }
})

-- LSP settings
local nvim_lsp = require("lspconfig")
local on_attach = function(_, bufnr)
  -- Setting lsp completion with omnifunc
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  local map = vim.api.nvim_buf_set_keymap
  local opts = { noremap = true, silent = true }

  map(bufnr, "n", "gd", ":lua vim.lsp.buf.definition()<Enter>", opts)
  map(bufnr, "n", "K", ":lua vim.lsp.buf.hover()<Enter>", opts)
  map(bufnr, "n", "gi", ":lua vim.lsp.buf.implementation()<Enter>", opts)
  map(bufnr, "n", "gr", ":lua vim.lsp.buf.references()<Enter>", opts)
  map(bufnr, "n", "<leader>d", ":lua vim.lsp.diagnostic.show_line_diagnostics()<Enter>", opts)
  map(bufnr, "n", "<leader>rn", ":lua vim.lsp.buf.rename()<Enter>", opts)
  map(bufnr, "n", "<leader>ca", ":lua vim.lsp.buf.code_action()<Enter>", opts)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local servers = {
  "clangd", "tsserver", "elixirls", "bashls", "vimls", "jsonls", "html", "cssls", "ocamllsp", "bashls"
}

for _, server in ipairs(servers) do
  nvim_lsp[server].setup({
    on_attach = on_attach,
    capabilities = capabilities
  })
end

local sumneko_binary = "/usr/bin/lua-language-server"

nvim_lsp.sumneko_lua.setup {
  cmd = { sumneko_binary },
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
        -- Setup your lua path
        path = vim.split(package.path, ";")
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { 'vim' },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file('', true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = true,
  signs = true,
  underline = true,
  update_in_insert = true,
})

vim.g.UltiSniptsEditSplit = "normal"
vim.g.UltiSnipsSnippetDirectories = { "ultisnips" }
vim.g.UltiSnipsExpandTrigger = "<C-e>"
vim.g.UltiSnipsJumpForwardTrigger = "<Tab>"
vim.g.UltiSnipsJumpBackwardTrigger = "<S-tab>"

local cmp = require("cmp")

cmp.setup({
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
      ["<C-j>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
      ["<C-k>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
      ["<C-Space>"] = cmp.mapping.complete(),
      ["<C-e>"] = cmp.mapping.close(),
      ["<CR>"] = cmp.mapping.confirm({ select = true }),
    },
    sources = {
      { name = "nvim_lua" },
      { name = "nvim_lsp" },
      { name = "path" },
      { name = "ultisnips" },
    },
    experimental = {
      native_menu = false,
      ghost_text = true,
    },
})
