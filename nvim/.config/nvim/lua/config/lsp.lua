local nvim_lsp = require "lspconfig"
local border = "single"

local diagnostics_opts = {
  virtual_text = { spacing = 4, prefix = "●" },
  signs = true,
  underline = true,
  update_in_insert = true,
  source = "if_many",
}

-- Signs
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- Handlers
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border, max_width = 100, max_height = 100 })
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border })
vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, diagnostics_opts)

-- Null-ls
local null_ls = require "null-ls"

null_ls.config {
  sources = {
    null_ls.builtins.formatting.prettier.with {
      filetypes = {
        "javascript",
        "javascriptreact",
        "typescript",
        "typescriptreact",
        "vue",
        "svelte",
        "css",
        "scss",
        "less",
        "html",
        "json",
        "yaml",
        "graphql",
      },
    },
    null_ls.builtins.formatting.mix,
    null_ls.builtins.formatting.sqlformat,
    null_ls.builtins.formatting.stylua,
    null_ls.builtins.diagnostics.credo,
    null_ls.builtins.diagnostics.eslint_d,
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.diagnostics.write_good,
  },
}

local on_attach = function(client, bufnr)
  -- Setting lsp completion with omnifunc
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  -- Mappings
  local map = vim.api.nvim_buf_set_keymap
  local opts = { noremap = true, silent = true }

  map(bufnr, "n", "gd", ":lua vim.lsp.buf.definition()<Enter>", opts)
  map(bufnr, "n", "K", ":lua vim.lsp.buf.hover()<Enter>", opts)
  map(bufnr, "n", "gi", ":lua vim.lsp.buf.implementation()<Enter>", opts)
  map(bufnr, "n", "gr", ":lua vim.lsp.buf.references()<Enter>", opts)
  map(bufnr, "n", "<leader>d", ":lua vim.lsp.diagnostic.show_line_diagnostics()<Enter>", opts)
  map(bufnr, "n", "<leader>rn", ":lua vim.lsp.buf.rename()<Enter>", opts)
  map(bufnr, "n", "<leader>ca", ":lua vim.lsp.buf.code_action()<Enter>", opts)

  -- Enabling formatting
  if client.name == "null-ls" or client.name == "clangd" then
    map(bufnr, "n", "<C-f>", ":lua vim.lsp.buf.formatting()<Enter>", opts)

    -- vim.cmd [[
    -- augroup LspFormatting
    --   autocmd!
    --   autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting()
    -- augroup end
    -- ]]
  else
    client.resolved_capabilities.document_formatting = false
    client.resolved_capabilities.document_range_formatting = false
  end
end

local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

local servers = {
  "null-ls",
  "clangd",
  "tsserver",
  "vimls",
  "jsonls",
  "html",
  "cssls",
  "ocamllsp",
}

for _, server in ipairs(servers) do
  nvim_lsp[server].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
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
        version = "LuaJIT",
        -- Setup your lua path
        path = vim.split(package.path, ";"),
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

nvim_lsp.elixirls.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  cmd = { vim.fn.expand "~/repos/elixir-ls/language_server.sh" },
  settings = {
    elixirLS = {
      dialyzerEnabled = false,
    },
  },
}

nvim_lsp.bashls.setup {
  on_attach = on_attach,
  capabilities = capabilities,
  filetypes = { "sh", "zsh" },
}
