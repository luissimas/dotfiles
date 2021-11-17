local function lsp_client()
  local clients = vim.lsp.get_active_clients()

  if not next(clients) then
    return " "
  end

  local msg = ""
  for _, client in ipairs(clients) do
    msg = msg .. " " .. client.name
  end

  return msg
end

local function diff()
  local gitsigns = vim.b.gitsigns_status_dict

  if gitsigns then
    return {
      added = gitsigns.added,
      modified = gitsigns.changed,
      removed = gitsigns.removed,
    }
  end
end

local theme = require("colorscheme").lualine_theme or "auto"

require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = theme,
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = { "" },
    always_divide_middle = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { { "b:gitsigns_head", icon = "" }, { "diff", source = diff } },
    lualine_c = { { "filename", path = 1 } },
    lualine_x = { { "diagnostics", sources = { "nvim_lsp" } }, { lsp_client } },
    lualine_y = { "filetype" },
    lualine_z = { "progress", "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  extensions = { "quickfix" },
}
