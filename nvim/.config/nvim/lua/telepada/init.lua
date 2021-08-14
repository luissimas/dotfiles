-- Custom telescope functions related to finding files in different directories

local M = {}

-- Custom Ivy theme
function M.ivy(config, height)
  local defaults = {
    layout_strategy = "bottom_pane",
    layout_config = {
      height = height or 20,
    },
  }

  local opts = vim.tbl_deep_extend("force", defaults, config or {})

  local theme = require("telescope.themes").get_ivy(opts)

  return theme
end

-- Find files with git_files and falling back to git_files
function M.find_files()
  local ok = pcall(require("telescope.builtin").git_files, M.ivy())

  if not ok then
    require("telescope.builtin").find_files(M.ivy())
  end
end

-- Find in dotfiles custom picker
function M.find_dotfiles()
  require("telescope.builtin").git_files(M.ivy({
    prompt_title = "Dotfiles",
    cwd = "~/dotfiles",
  }))
end

-- Find in home directory
function M.find_home()
  require("telescope.builtin").find_files(M.ivy({
    prompt_title = "All files",
    cwd = "~",
  }))
end

-- Find in my vault directory
function M.find_vault()
  require("telescope.builtin").find_files(M.ivy({
    prompt_title = "Notes",
    cwd = "~/dox/vault",
  }))
end

-- Live grep
function M.live_grep()
  require("telescope.builtin").live_grep(M.ivy())
end

-- Old files
function M.old_files()
  require("telescope.builtin").oldfiles(M.ivy())
end

-- Search in buffer
function M.find_buffer()
  require("telescope.builtin").current_buffer_fuzzy_find(M.ivy({
    sorting_strategy = "ascending",
    layout_config = {
      prompt_position = "top",
    },
  }))
end

-- Buffer switcher with ivy theme
function M.switch_buffer()
  require("telescope.builtin").buffers(M.ivy({}))
end

-- Emacs-like M-x
function M.mx()
  require("telescope.builtin").commands(M.ivy({}, 8))
end

return M
