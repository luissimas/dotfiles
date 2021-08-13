-- Options
vim.o.completeopt = "menuone,noinsert,noselect"

-- Main config
require("compe").setup({
  enabled = true,
  autocomplete = true,
  debug = false,
  min_length = 1,
  preselect = "enable",
  throttle_time = 80,
  source_timeout = 200,
  incomplete_delay = 400,
  max_abbr_width = 100,
  max_kind_width = 100,
  max_menu_width = 100,
  documentation = {
    border = "none",
    winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
    max_width = 120,
    min_width = 60,
    max_height = math.floor(vim.o.lines * 0.3),
    min_height = 1,
  },
  -- Sources for completion
  source = {
    path = true,
    spell = true,
    buffer = false,
    tags = false,
    calc = true,
    omni = false,
    emoji = false,
    nvim_lsp = true,
    nvim_lua = true,
    vsnip = false,
    ultisnips = true,
    treesitter = false,
    tabnine = false,
    orgmode = true,
  },
})

-- Completion toggle
_G.compe_popup_toggle = function()
  if vim.fn.pumvisible() == 1 then
    return vim.fn.call("compe#close", {})
  else
    return vim.fn.call("compe#complete", {})
  end
end

-- Next/prev completion items keymaps
vim.api.nvim_set_keymap("i", "<C-j>", "<C-n>", { noremap = true, expr = false })
vim.api.nvim_set_keymap("s", "<C-j>", "<C-n>", { noremap = true, expr = false })
vim.api.nvim_set_keymap("i", "<C-k>", "<C-p>", { noremap = true, expr = false })
vim.api.nvim_set_keymap("s", "<C-k>", "<C-p>", { noremap = true, expr = false })

-- Manual completion popup toggle
vim.api.nvim_set_keymap("i", "<C-Space>", "v:lua.compe_popup_toggle()", { noremap = true, expr = true, silent = true })

-- Confirm
vim.api.nvim_set_keymap("i", "<CR>", "compe#confirm('<CR>')", { noremap = true, expr = true, silent = true })
