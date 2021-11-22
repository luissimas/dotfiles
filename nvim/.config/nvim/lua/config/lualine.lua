local function lsp_client()
  local clients = vim.lsp.get_active_clients()
  if not next(clients) then
    return " "
  end

  for _, client in ipairs(clients) do
    if client.name ~= "null-ls" then
      return " " .. client.name
    end
  end

  return " "
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

local function enableDiff()
  local filetype = vim.bo.filetype
  local disabled = { "markdown", "org" }

  for _, ft in ipairs(disabled) do
    if filetype == ft then
      return false
    end
  end

  return true
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
    lualine_b = { { "b:gitsigns_head", icon = "" }, { "diff", source = diff, cond = enableDiff } },
    lualine_c = { { "filename", path = 1 } },
    lualine_x = { { "diagnostics", sources = { "nvim_lsp" } }, { lsp_client } },
    lualine_y = { "filetype" },
    lualine_z = { "progress", "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { { "filename", path = 1 } },
    lualine_x = {},
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  extensions = { "quickfix" },
}
