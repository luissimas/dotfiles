-- Custom telescope functions related to setting colorschemes

local M = {}


-- Function to change system's colorscheme
function change_theme(theme, method)

  if method == 'wal' then
  -- Wal command
  vim.fn.system("wal -i "..theme.." -q")

  -- Changing colorscheme and disabling termguicolors
  vim.cmd('colorscheme wal')
  vim.cmd('set termguicolors&')

  require('config.colorscheme').colorscheme = 'wal'

  elseif method == 'name' then
    -- Change wall theme and set specific wallpapers for each theme
    if theme == 'gruvbox' then
      vim.fn.system("wal --theme base16-gruvbox-hard -q")
      vim.fn.system("feh --bg-fill ~/.wal/forestfog.jpg")
    elseif theme == 'nord' then
      vim.fn.system("wal --theme base16-nord -q")
      vim.fn.system("feh --bg-fill ~/.wal/graymountain.jpeg")
    elseif theme == 'onedark' then
      vim.fn.system("wal --theme base16-onedark -q")
      vim.fn.system("feh --bg-fill ~/.wal/japanesestreet.png")
    elseif theme == 'dracula' then
      vim.fn.system("wal --theme base16-dracula -q")
      vim.fn.system("feh --bg-fill ~/.wal/basedracula.png")
    elseif theme == 'palenight' then
      vim.fn.system("wal --theme base16-material-palenight -q")
      vim.fn.system("feh --bg-fill ~/.wal/palenight.jpg")
    end

    -- Change vim theme
    vim.cmd('set termguicolors')
    vim.cmd('colorscheme '..theme)
  end
end


--
-- Pickers
--

-- Pywal
function M.wal()
  require('telescope.builtin').find_files(require('telescope.themes').get_dropdown({
    prompt_title = "Image",

    -- No preview
    previewer = false,

    -- Clear ignore patterns filter (so we can find the images)
    file_ignore_patterns = {},

    cwd = "~/.wal/",

    attach_mappings = function(prompt_bufnr, map)
      function set_theme(close)
        local file = require('telescope.actions.state').get_selected_entry(bufnr)

        change_theme(file.cwd..file.value, 'wal')
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
  }))
end

-- Neovim's colorschemes
function M.colorscheme()
  require('telescope.builtin').colorscheme(require('telescope.themes').get_dropdown({

    attach_mappings = function(prompt_bufnr, map)
      function set_colorscheme(close)

        local themeName = require('telescope.actions.state').get_selected_entry(bufnr)

        change_theme(themeName.value, 'name')

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
  }))
end

return M
