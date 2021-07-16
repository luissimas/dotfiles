require("tmux").setup({
  navigation = {
    -- cycles to opposite pane while navigating into the border
    cycle_navigation = false,
    -- enables default keybindings (C-hjkl) for normal mode
    enable_default_keybindings = true,
    -- prevents unzoom tmux when navigating beyond vim border
    persist_zoom = true,
  },
  resize = {
    -- enables default keybindings (A-hjkl) for normal mode
    enable_default_keybindings = true,
    -- sets resize steps for x axis
    resize_step_x = 5,
    -- sets resize steps for y axis
    resize_step_y = 5,
  },
})
