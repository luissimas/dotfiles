--
--              _                  _ _
--   __ _  __ _| | __ ___  ___   _| (_)_ __   ___
--  / _` |/ _` | |/ _` \ \/ / | | | | | '_ \ / _ \
-- | (_| | (_| | | (_| |>  <| |_| | | | | | |  __/
--  \__, |\__,_|_|\__,_/_/\_\\__, |_|_|_| |_|\___|
--  |___/                    |___/
--
--


local section = require('galaxyline').section

--[[
local colors = {
    bg = '#3B4252',
    fg = '#ECEFF4',
    yellow = '#FFCC66',
    cyan = '#4EC9B0',
    green = '#608B4E',
    orange = '#FF8800',
    purple = '#C586C0',
    magenta = '#D16D9E',
    blue = '#569CD6',
    red = '#F44747'
}
]]

-- Onedark
local colors = {
    bg = '#282c34',
    fg = '#ECEFF4',
    yellow = '#FFCC66',
    cyan = '#4EC9B0',
    green = '#608B4E',
    orange = '#FF8800',
    purple = '#C586C0',
    magenta = '#D16D9E',
    blue = '#569CD6',
    red = '#F44747'
}


local condition = require('galaxyline.condition')

section.short_line_list = {'NvimTree', 'vista', 'dbui', 'packer'}

section.left[1] = {
    ViMode = {
        provider = function()
            -- auto change color according the vim mode
            local mode_color = {
                n = colors.blue,
                i = colors.green,
                v = colors.purple,
                [''] = colors.purple,
                V = colors.purple,
                c = colors.magenta,
                no = colors.blue,
                s = colors.orange,
                S = colors.orange,
                [''] = colors.orange,
                ic = colors.yellow,
                R = colors.red,
                Rv = colors.red,
                cv = colors.blue,
                ce = colors.blue,
                r = colors.cyan,
                rm = colors.cyan,
                ['r?'] = colors.cyan,
                ['!'] = colors.blue,
                t = colors.blue
            }

            vim.api.nvim_command('hi GalaxyViMode guibg=' .. mode_color[vim.fn.mode()])
            return ' '
        end,
    }
}

section.left[2] = {
  FileName = {
    provider = function()
      return '  '..vim.fn.expand('%:t')..' '
    end,

    highlight = { colors.fg, colors.bg },
  }
}

section.left[3] = {
    GitIcon = {
        provider = function()
            return ' '
        end,
        condition = condition.check_git_workspace,
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.orange, colors.bg}
    }
}

section.left[4] = {
    GitBranch = {
        provider = 'GitBranch',
        condition = condition.check_git_workspace,
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.fg, colors.bg}
    }
}

section.left[5] = {
    DiffAdd = {
        provider = 'DiffAdd',
        condition = condition.hide_in_width,
        icon = '  ',
        highlight = {colors.green, colors.bg}
    }
}

section.left[6] = {
    DiffModified = {
        provider = 'DiffModified',
        condition = condition.hide_in_width,
        icon = ' 柳',
        highlight = {colors.blue, colors.bg}
    }
}

section.left[7] = {
    DiffRemove = {
        provider = 'DiffRemove',
        condition = condition.hide_in_width,
        icon = '  ',
        highlight = {colors.red, colors.bg}
    }
}

section.right[1] = {
    DiagnosticError = {provider = 'DiagnosticError', icon = '  ', highlight = {colors.red, colors.bg}}
}

section.right[2] = {
  DiagnosticWarn = {
    provider = 'DiagnosticWarn', icon = '  ', highlight = {colors.orange, colors.bg}
  }
}

section.right[3] = {
    DiagnosticHint = {provider = 'DiagnosticHint', icon = '  ', highlight = {colors.blue, colors.bg}}
}

section.right[4] = {
  DiagnosticInfo = {
    provider = 'DiagnosticInfo', icon = '  ', highlight = {colors.yellow, colors.bg}
  }
}

section.right[5] = {
    ShowLspClient = {
        provider = function()
          local lspclient = require('galaxyline.provider_lsp').get_lsp_client()

          if lspclient == "No Active Lsp" then
            return ' '
          else
            return ' '..lspclient
          end

        end,

        highlight = {colors.fg, colors.bg}
    }
}

section.right[6] = {
    LineInfo = {
        provider = 'LineColumn',
        separator = '  ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.fg, colors.bg}
    }
}

section.right[7] = {
    PerCent = {
        provider = 'LinePercent',
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.fg, colors.bg}
    }
}

section.right[8] = {
    BufferType = {
        provider = 'FileTypeName',
        condition = condition.hide_in_width,
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.fg, colors.bg}
    }
}

section.right[9] = {
    FileEncode = {
        provider = 'FileEncode',
        condition = condition.hide_in_width,
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.fg, colors.bg}
    }
}

section.right[10] = {
    Space = {
        provider = function()
            return ' '
        end,
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.orange, colors.bg}
    }
}

section.short_line_left[1] = {
    BufferType = {
        provider = 'FileTypeName',
        separator = ' ',
        separator_highlight = {'NONE', colors.bg},
        highlight = {colors.fg, colors.bg}
    }
}

section.short_line_left[2] = {
    SFileName = {
      provider = 'SFileName', condition = condition.buffer_not_empty, highlight = {colors.fg, colors.bg}
    }
}

section.short_line_right[1] = {
  BufferIcon = {
    provider = 'BufferIcon', highlight = {colors.fg, colors.bg}
  }
}
