--
--  _                              __ _
-- | |___ _ __     ___ ___  _ __  / _(_) __ _
-- | / __| '_ \   / __/ _ \| '_ \| |_| |/ _` |
-- | \__ \ |_) | | (_| (_) | | | |  _| | (_| |
-- |_|___/ .__/   \___\___/|_| |_|_| |_|\__, |
--       |_|                            |___/
--
--[[
 Importing the setup configuration for each language server.

 All the information about the servers can be found here:
 https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#html
-- ]]
-- Capabilities for snippet support in servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

-- Lspconfig's util module
local util = require("lspconfig.util")

-- Eslint options
local eslint = {
  lintCommand = "eslint_d -f unix --stdin --stdin-filename ${INPUT}",
  lintStdin = true,
  lintFormats = { "%f:%l:%c: %m" },
  lintIgnoreExitCode = true,
  formatCommand = "eslint_d --fix-to-stdout --stdin --stdin-filename=${INPUT}",
  formatStdin = true,
  rootMarkers = { "package.json", ".git/" },
}

-- Credo options
local credo = {
  lintCommand = "MIX_ENV=test mix credo suggest --format=flycheck --read-from-stdin ${INPUT}",
  lintStdin = true,
  lintFormats = { "%f:%l:%c: %t: %m", "%f:%l: %t: %m" },
  rootMarkers = { "mix.exs" },
}

-- EFM language server
require("lspconfig").efm.setup({
  init_options = { documentFormatting = true },
  filetypes = { "javascript", "typescript", "elixir" },
  root_dir = util.root_pattern("package.json", ".git", "mix.exs"),
  settings = {
    languages = {
      javascript = { eslint },
      typescript = { eslint },
      elixir = { credo },
    },
  },
})

-- Elixir (https://www.mitchellhanberg.com/how-to-set-up-neovim-for-elixir-development/)
require("lspconfig").elixirls.setup({
  cmd = { vim.fn.expand("~/repos/elixir-ls/language_server.sh") },
  filetypes = { "elixir", "eelixir" },
  root_dir = util.root_pattern("mix.exs", ".git") or vim.loop.os_homedir(),
  settings = {
    elixirLS = {
      dialyzerEnabled = true,
    },
  },
})

-- JavaScript/TypeScript
require("lspconfig").tsserver.setup({
  cmd = { "typescript-language-server", "--stdio" },
  filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
  root_dir = util.root_pattern("package.json", "tsconfig.json", "jsconfig.json", ".git"),
})

-- Lua
local sumneko_binary = "/usr/bin/lua-language-server"
local luadev = require("lua-dev").setup({
  library = {
    vimruntime = true, -- runtime path
    types = true, -- full signature, docs and completion of vim.api, vim.treesitter, vim.lsp and others
    plugins = true, -- installed opt or start plugins in packpath
    -- you can also specify the list of plugins to make available as a workspace library
    -- plugins = { "nvim-treesitter", "plenary.nvim", "telescope.nvim" },
  },
  -- pass any additional options that will be merged in the final lsp config
  lspconfig = { cmd = { sumneko_binary } },
})

require("lspconfig").sumneko_lua.setup(luadev)

-- require("lspconfig").sumneko_lua.setup {
--   cmd = {sumneko_binary},
--   settings = {
--     Lua = {
--       runtime = {
--         -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--         version = "LuaJIT",
--         -- Setup your lua path
--         path = vim.split(package.path, ";")
--       },
--       diagnostics = {
--         -- Get the language server to recognize the `vim` global
--         globals = {"vim"}
--       },
--       workspace = {
--         -- Make the server aware of Neovim runtime files
--         library = {
--           [vim.fn.expand("$VIMRUNTIME/lua")] = true,
--           [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
--           -- Adding nvim lua api types
--           [vim.fn.expand("$HOME/.local/share/nvim/site/pack/packer/start/lua-dev.nvim/types")] = true
--         }
--       },
--       -- Do not send telemetry data containing a randomized but unique identifier
--       telemetry = {enable = false}
--     }
--   }
-- }

-- Python
require("lspconfig").pyright.setup({})

-- Bash
require("lspconfig").bashls.setup({
  cmd = { "bash-language-server", "start" },
  filetypes = { "sh", "zsh" },
})

-- C/C++
require("lspconfig").clangd.setup({
  cmd = { "clangd", "--background-index" },
  filetypes = { "c", "cpp", "objc", "objcpp", "ch" },
  capabilities = capabilities,
})

-- Haskell
-- require("lspconfig").hls.setup {
--   cmd = {"haskell-language-server-wrapper", "--lsp"},
--   filetypes = {"haskell", "lhaskell"},
--   lspinfo = function(cfg)
--     -- return "specific"
--     if cfg.settings.languageServerHaskell.logFile or false then
--       return "logfile: " .. cfg.settings.languageServerHaskell.logFile
--     end
--     return ""
--   end,
--   root_dir = util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
--   settings = {
--     languageServerHaskell = {
--       formattingProvider = "ormolu"
--     }
--   }
-- }

-- Vim script
require("lspconfig").vimls.setup({})

-- Json
require("lspconfig").jsonls.setup({})

-- R
require("lspconfig").r_language_server.setup({})

-- HTML and CSS
require("lspconfig").html.setup({ capabilities = capabilities })
require("lspconfig").cssls.setup({ capabilities = capabilities })

-- Docker
require("lspconfig").dockerls.setup({
  cmd = { "docker-langserver", "--stdio" },
  filetypes = { "Dockerfile", "dockerfile" },
})

-- Latex
require("lspconfig").texlab.setup({
  cmd = { "texlab" },
  filetypes = { "tex", "bib" },
  settings = {
    texlab = {
      auxDirectory = ".",
      bibtexFormatter = "texlab",
      build = {
        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
        executable = "latexmk",
        isContinuous = false,
      },
      chktex = { onEdit = false, onOpenAndSave = false },
      diagnosticsDelay = 300,
      formatterLineLength = 80,
      forwardSearch = { args = {} },
    },
  },
})
