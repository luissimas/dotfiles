require "toggleterm".setup {
  size = 10,
  --open_mapping = [[<leader>t]],
  open_mapping = [[<C-t>]],
  hide_numbers = true,
  insert_mappings = true,
  shade_filetypes = {},
  shade_terminals = true,
  shading_factor = 0.5, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  persist_size = false,
  direction = "horizontal", -- 'vertical' | 'horizontal' | 'window' | 'float'
  close_on_exit = true
}
