require"toggleterm".setup{
  size = 10,
  --open_mapping = [[<leader>t]],
  open_mapping = [[<C-t>]],
  hide_numbers = true,
  insert_mappings = false,
  shade_filetypes = {},
  shade_terminals = false,
  shading_factor = '<number>', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  persist_size = false,
  direction = 'horizontal',
  close_on_exit = true
}
