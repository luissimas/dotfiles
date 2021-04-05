--
--  _       _                      _
-- | |_ ___| | ___ _ __   __ _  __| | __ _
-- | __/ _ \ |/ _ \ '_ \ / _` |/ _` |/ _` |
-- | ||  __/ |  __/ |_) | (_| | (_| | (_| |
--  \__\___|_|\___| .__/ \__,_|\__,_|\__,_|
--                |_|
--
--
-- This is a file for my custom pickers for telescope
--

--
-- TODO: CLEAN THIS FILE!!!!!!!!!
--


-- Module return name
-- Is this a table?
local M = {}


-- Find in dotfiles custom picker
function M.find_dotfiles()
   require('telescope.builtin').git_files{
     prompt_title = "Dotfiles",
     cwd = "~/dotfiles/"
   }
end


-- Find in home directory
function M.find_home()
   require('telescope.builtin').find_files{
     prompt_title = "All files",
     cwd = "~/"
   }
end

function M.find_vault()
  require('telescope.builtin').find_files{
    prompt_title = "Notes",
    cwd = "~/dox/vault"
  }
end

-- Change theme using pywal
function change_theme(file)

  -- Wal command
  vim.fn.system("wal -i "..file.." -q")

  -- Changing colorscheme and disabling termguicolors
  vim.cmd('colorscheme wal')
  vim.cmd('set termguicolors&')
end

function M.wal()
  require('telescope.builtin').find_files{
    prompt_title = "Image",

    -- Clear ignore patterns filter (so we can find the images)
    file_ignore_patterns = { " " },

    cwd = "~/.wal/",

    attach_mappings = function(prompt_bufnr, map)
      function set_theme(close)
        local file = require('telescope.actions.state').get_selected_entry(bufnr)

        change_theme(file.cwd..file.value)
        --print(file.cwd..file.value)

        if close then
          require('telescope.actions').close(prompt_bufnr)
        end
      end

      map('i', '<CR>', function(bufnr)
        set_theme(true)
      end
      )

      map('n', '<CR>', function(bufnr)
        set_theme(true)
      end
      )

      return true
    end,
  }
end

-- Change vim and pc theme
function change_colorscheme(themeName)

  print(themeName)

  -- Change wall theme and set specific wallpapers for each theme
  if themeName == 'gruvbox' then
    vim.fn.system("wal --theme base16-gruvbox-hard -q")
    vim.fn.system("feh --bg-fill ~/.wal/forestfog.jpg")
  elseif themeName == 'nord' then
    vim.fn.system("wal --theme base16-nord -q")
    vim.fn.system("feh --bg-fill ~/.wal/graymountain.jpeg")
  elseif themeName == 'onedark' then
    vim.fn.system("wal --theme base16-onedark -q")
    vim.fn.system("feh --bg-fill ~/.wal/japanesestreet.png")
  end

  -- Change vim theme
  vim.cmd('set termguicolors')
  vim.cmd('colorscheme '..themeName)
end

function M.colorscheme()
  require('telescope.builtin').colorscheme{

    attach_mappings = function(prompt_bufnr, map)
      function set_colorscheme(close)

        change_colorscheme(require('telescope.actions.state').get_selected_entry(bufnr).value)


        if close then
          require('telescope.actions').close(prompt_bufnr)
        end
      end


      map('i', '<CR>', function(bufnr)
        set_colorscheme(true)
      end
      )

      map('n', '<CR>', function(bufnr)
        set_colorscheme(true)
      end
      )

      return true
    end,
  }
end

-- Returning all methods associated with module
-- Is this a table?
return M
