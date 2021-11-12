local cmp = require "cmp"

cmp.setup {
  completion = {
    keyword_length = 1,
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
    { name = "orgmode" },
    { name = "vim-dadbod-completion" },
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
        orgmode = "[Org]",
        ["vim-dadbod-completion"] = "[DB]",
      },
    },
  },
  experimental = {
    native_menu = false,
    ghost_text = true,
  },
}
