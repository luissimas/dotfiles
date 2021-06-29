-- Custom telescope functions related to finding files in different directories

local M = {}

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

return M
