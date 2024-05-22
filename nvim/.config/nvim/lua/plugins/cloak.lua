return {
  'laytan/cloak.nvim',
  lazy = false,
  opts = {
    -- NOTE: Doesn't work with the 0.1.x version of telescope: https://github.com/laytan/cloak.nvim/issues/18
    cloak_telescope = true,
    patterns = {
      {
        -- Match any file starting with '.env'.
        -- This can be a table to match multiple file patterns.
        file_pattern = '.env*',
        -- Match an equals sign and any character after it.
        -- This can also be a table of patterns to cloak,
        -- example: cloak_pattern = { ':.+', '-.+' } for yaml files.
        cloak_pattern = { '=.+', ':.+', '-.+' },
        -- A function, table or string to generate the replacement.
        -- The actual replacement will contain the 'cloak_character'
        -- where it doesn't cover the original text.
        -- If left empty the legacy behavior of keeping the first character is retained.
        replace = nil,
      },
    },
  },
  keys = {
    { '<leader>tc', '<cmd>CloakToggle<CR>', desc = '[T]oggle [C]loak' },
  },
}
