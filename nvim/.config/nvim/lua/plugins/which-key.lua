-- Show keybinds.
return {
  'folke/which-key.nvim',
  event = 'VimEnter', -- Sets the loading event to 'VimEnter'
  config = function() -- This is the function that runs, AFTER loading
    require('which-key').setup()

    -- Document existing key chains
    require('which-key').add {
      { '<leader>b', group = '[B]buffer' },
      { '<leader>c', group = '[C]ode' },
      { '<leader>d', group = '[D]ocument' },
      { '<leader>f', group = '[F]ind' },
      { '<leader>g', group = '[G]it' },
      { '<leader>h', group = '[H]arpoon' },
      { '<leader>o', group = '[O]pen' },
      { '<leader>q', group = '[Q]uickfix' },
      { '<leader>r', group = '[R]ename' },
      { '<leader>t', group = '[T]oggle' },
      { '<leader>w', group = '[W]orkspace' },
    }
    -- visual mode
    require('which-key').add {
      { '<leader>g', desc = '[G]it', mode = 'v' },
    }
  end,
}
