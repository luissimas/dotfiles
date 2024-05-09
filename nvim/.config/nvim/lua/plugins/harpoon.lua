return {
  'ThePrimeagen/harpoon',
  branch = 'harpoon2',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    local harpoon = require 'harpoon'

    harpoon:setup {
      settings = {
        save_on_toggle = true,
        sync_on_ui_close = true,
      },
    }

    vim.keymap.set('n', '<leader>hh', function()
      harpoon:list():add()
    end, { desc = '[H]arpoon' })

    vim.keymap.set('n', '<leader>hl', function()
      harpoon.ui:toggle_quick_menu(harpoon:list())
    end, { desc = '[H]arpoon [L]ist' })

    vim.keymap.set('n', '<leader>ha', function()
      harpoon:list():select(1)
    end, { desc = 'Harpoon 1' })

    vim.keymap.set('n', '<leader>hs', function()
      harpoon:list():select(2)
    end, { desc = 'Harpoon 2' })

    vim.keymap.set('n', '<leader>hd', function()
      harpoon:list():select(3)
    end, { desc = 'Harpoon 3' })

    vim.keymap.set('n', '<leader>hf', function()
      harpoon:list():select(4)
    end, { desc = 'Harpoon 4' })

    -- Toggle previous & next buffers stored within Harpoon list
    vim.keymap.set('n', '<leader>hp', function()
      harpoon:list():prev()
    end, { desc = '[H]arpoon [P]revious' })
    vim.keymap.set('n', '<leader>hn', function()
      harpoon:list():next()
    end, { desc = '[H]arpoon [N]ext' })
  end,
}
