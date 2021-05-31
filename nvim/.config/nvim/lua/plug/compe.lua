-- Options
vim.o.completeopt = "menuone,noinsert,noselect"

-- Main config
require("compe").setup {
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
  documentation = true,
  -- Sources for completion
  source = {
    path = true,
    spell = false,
    buffer = false,
    tags = true,
    calc = true,
    omni = false,
    emoji = false,
    nvim_lsp = true,
    nvim_lua = true,
    vsnip = true,
    ultisnips = false,
    treesitter = false
  }
}

-- Enabling lspkind icons
require("lspkind").init()

-- Vsnip integration
local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
  local col = vim.fn.col(".") - 1
  if col == 0 or vim.fn.getline("."):sub(col, col):match("%s") then
    return true
  else
    return false
  end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn.call("vsnip#available", {1}) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn["compe#complete"]()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    -- If <S-Tab> is not working in your terminal, change it to <C-h>
    return t "<S-Tab>"
  end
end

-- Completion toggle
_G.compe_popup_toggle = function()
  if vim.fn.pumvisible() == 1 then
    return vim.fn.call("compe#close", {})
  else
    return vim.fn.call("compe#complete", {})
  end
end

-- Tab keymaps
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

-- Manual completion popup toggle
vim.api.nvim_set_keymap("i", "<C-Space>", "v:lua.compe_popup_toggle()", {noremap = true, expr = true, silent = true})

-- Confirm
vim.api.nvim_set_keymap("i", "<CR>", "compe#confirm('<CR>')", {noremap = true, expr = true, silent = true})
