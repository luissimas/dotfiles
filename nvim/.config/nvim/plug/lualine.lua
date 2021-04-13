--
--  _             _ _
-- | |_   _  __ _| (_)_ __   ___
-- | | | | |/ _` | | | '_ \ / _ \
-- | | |_| | (_| | | | | | |  __/
-- |_|\__,_|\__,_|_|_|_| |_|\___|
--
--



-- Custom functions
-- Lsp client function from Galaxyline.nvim
local get_lsp_client = function (msg)
  msg = 'No Active Lsp'
  local buf_ft = vim.api.nvim_buf_get_option(0,'filetype')
  local clients = vim.lsp.get_active_clients()

  if next(clients) == nil then
    return msg
  end

  for _,client in ipairs(clients) do
    local filetypes = client.config.filetypes
    if filetypes and vim.fn.index(filetypes,buf_ft) ~= -1 then
      return client.name
    end
  end

  return msg
end





require('lualine').setup{
  options = {
    theme = require('colorscheme').colorscheme,
    section_separators = {'', ''},
    component_separators = {'', ''},
  },

  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch'},
    lualine_c = {'filename'},
    lualine_x = { {'diagnostics', sources={'nvim_lsp'} }, { get_lsp_client, icon=''} },
    lualine_y = {'filetype', 'progress', 'location'},
    lualine_z = {'encoding'}
  },
}

