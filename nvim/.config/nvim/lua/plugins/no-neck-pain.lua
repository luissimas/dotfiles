-- Center buffer contents
return {
  "shortcuts/no-neck-pain.nvim",
  version = "*",
  config = function()
    vim.keymap.set("n", "<leader>tn", "<cmd>NoNeckPain<CR>", { desc = "[T]oggle [N]oNeckPain" })
  end,
}
