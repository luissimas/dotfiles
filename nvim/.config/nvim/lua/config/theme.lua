local M = {}

M.families = {
  gruvbox = { dark = "gruvbox", light = "gruvbox" },
  github = { dark = "github_dark", light = "github_light_default" },
  tokyonight = { dark = "tokyonight-night", light = "tokyonight-day" },
  catppuccin = { dark = "catppuccin-mocha", light = "catppuccin-latte" },
  ["rose-pine"] = { dark = "rose-pine-moon", light = "rose-pine-dawn" },
}

M.active = { dark = "gruvbox", light = "github" }

local function detect_background()
  if vim.fn.has("mac") == 0 then
    return vim.o.background
  end
  local out = vim.fn.system("defaults read -g AppleInterfaceStyle 2>/dev/null")
  return out:match("Dark") and "dark" or "light"
end

function M.apply()
  local bg = detect_background()
  local family = M.active[bg]
  local scheme = M.families[family][bg]
  local bg_changed = vim.o.background ~= bg
  local scheme_changed = vim.g.colors_name ~= scheme
  if bg_changed then
    vim.opt.background = bg
  end
  -- Always reload the colorscheme when the background flips, even if the name
  -- is unchanged (e.g. gruvbox dark → gruvbox light). Otherwise highlights from
  -- the old palette leak through.
  if scheme_changed or bg_changed then
    vim.cmd.colorscheme(scheme)
  end
end

function M.pick()
  local names = vim.tbl_keys(M.families)
  table.sort(names)
  vim.ui.select(names, { prompt = "Theme family" }, function(choice)
    if not choice then
      return
    end
    M.active.dark, M.active.light = choice, choice
    M.apply()
  end)
end

return M
