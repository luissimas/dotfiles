local cmp = require("cmp")
local lspkind = require("lspkind")
local types = require("cmp.types")

local WIDE_HEIGHT = 40

cmp.setup({
  completion = {
    --autocomplete = false
    keyword_length = 3,
    completeopt = "menu,menuone,noinsert",
  },
  snippet = {

    expand = function(args)
      -- For `ultisnips` user.
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  formatting = {
    format = lspkind.cmp_format(),
  },
  documentation = {
    border = { "", "", "", " ", "", "", "", " " },
    winhighlight = "NormalFloat:NormalFloat,FloatBorder:NormalFloat",
    maxwidth = math.floor((WIDE_HEIGHT * 2) * (vim.o.columns / (WIDE_HEIGHT * 2 * 16 / 9))),
    maxheight = math.floor(WIDE_HEIGHT * (WIDE_HEIGHT / vim.o.lines)),
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
    { name = "nvim_lsp" },
    { name = "ultisnips" },
    { name = "buffer" },
    { name = "orgmode" },
    { name = "path" },
    { name = "nvim_lua" },
    { name = "calc" },
    { name = "calc" },
    { name = "spell" },
  },
  experimental = {
    native_menu = false,
    ghost_text = true,
  },
})
