require("lspsaga").init_lsp_saga({
  use_saga_diagnostic_sign = true,
  error_sign = "",
  warn_sign = "",
  hint_sign = "",
  infor_sign = "",
  dianostic_header_icon = " ",
  code_action_icon = " ",
  code_action_prompt = {
    enable = true,
    sign = true,
    sign_priority = 20,
    virtual_text = false,
  },
  finder_definition_icon = "  ",
  finder_reference_icon = "  ",
  max_preview_lines = 20, -- preview lines of lsp_finder and definition preview
  finder_action_keys = {
    open = "o",
    vsplit = "s",
    split = "x",
    quit = "q", -- quit can be a table
    scroll_down = "<C-j>",
    scroll_up = "<C-k>",
  },
  code_action_keys = {
    quit = "q",
    exec = "<CR>",
  },
  rename_action_keys = {
    quit = "<C-c>", -- quit can be a table
    exec = "<CR>",
  },
  definition_preview_icon = "  ",
  border_style = "single", -- "single" "double" "round" "plus"
  rename_prompt_prefix = "➤",
})

-- Keymaps
-- vim.api.nvim_set_keymap("n", "<leader>rn", ":Lspsaga rename<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "ca", ":Lspsaga code_action<Enter>", {noremap = true, silent = true})
-- vim.api.nvim_set_keymap("n", "K", ":Lspsaga hover_doc<Enter>", {noremap = true, silent = true})
