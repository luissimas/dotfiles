-- Custom telescope functions related to finding files in different directories

local M = {}

-- Custom Ivy theme
function M.ivy()
  local theme =
    require("telescope.themes").get_ivy(
    {
      layout_strategy = "bottom_pane",
      layout_config = {
        height = 8
      }
    }
  )

  return theme
end

-- Find files with git_files and falling back to git_files
function M.find_files()
  local ok = pcall(require("telescope.builtin").git_files)

  if not ok then
    require("telescope.builtin").find_files()
  end
end

-- Find in dotfiles custom picker
function M.find_dotfiles()
  require("telescope.builtin").git_files {
    prompt_title = "Dotfiles",
    cwd = "~/dotfiles"
  }
end

-- Find in home directory
function M.find_home()
  require("telescope.builtin").find_files {
    prompt_title = "All files",
    cwd = "~"
  }
end

-- Find in my vault directory
function M.find_vault()
  require("telescope.builtin").find_files {
    prompt_title = "Notes",
    cwd = "~/dox/vault"
  }
end

-- Search in buffer
function M.find_buffer()
  local opts = {
    sorting_strategy = "ascending",
    layout_config = {
      prompt_position = "top"
    }
  }

  require("telescope.builtin").current_buffer_fuzzy_find(opts)
end

-- Buffer switcher with ivy theme
function M.switch_buffer()
  require("telescope.builtin").buffers(M.ivy())
end

return M
