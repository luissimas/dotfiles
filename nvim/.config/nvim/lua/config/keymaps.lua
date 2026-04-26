-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local theme = require("config.theme")
vim.keymap.set("n", "<leader>uT", theme.apply, { desc = "Re-detect system theme" })
vim.keymap.set("n", "<leader>uF", theme.pick, { desc = "Pick theme family" })
