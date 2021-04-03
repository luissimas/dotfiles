--
--  _                              __ _
-- | |___ _ __     ___ ___  _ __  / _(_) __ _
-- | / __| '_ \   / __/ _ \| '_ \| |_| |/ _` |
-- | \__ \ |_) | | (_| (_) | | | |  _| | (_| |
-- |_|___/ .__/   \___\___/|_| |_|_| |_|\__, |
--       |_|                            |___/
--




--[[
 Importing the setup configuration for each language server and attaching
 completion on setup, so the completion plugin can actually display the
 information that the lsp server is returning.

 All the information about the servers can be found here:
 https://github.com/neovim/nvim-lspconfig/blob/master/CONFIG.md#html

]]

-- Bash
require('lspconfig').bashls.setup{on_attach=require'completion'.on_attach}

-- C/C++
require('lspconfig').clangd.setup{on_attach=require'completion'.on_attach}

-- Python
require('lspconfig').pyright.setup{on_attach=require'completion'.on_attach}

-- R
require('lspconfig').r_language_server.setup{on_attach=require'completion'.on_attach}

-- JavaScript/TypeScript
require('lspconfig').tsserver.setup{on_attach=require'completion'.on_attach}

-- Vim script
require('lspconfig').vimls.setup{on_attach=require'completion'.on_attach}

-- Json
require('lspconfig').jsonls.setup{on_attach=require'completion'.on_attach}

-- CSS
require('lspconfig').cssls.setup{on_attach=require'completion'.on_attach}

--[[
 HTML. For this one we have to set up snippets for it to work properly,
 that's why we have this lua block that loads the snippet support and pass it
 to the capabilities attribute of the server call
]]

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

require('lspconfig').html.setup{on_attach=require'completion'.on_attach, capabilities = capabilities}
