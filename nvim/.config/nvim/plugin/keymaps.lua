-- [[ Basic Keymaps ]]

-- Clear highlight on pressing <Esc> in normal mode
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous [D]iagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next [D]iagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic [E]rror messages' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Buffer navigation
vim.keymap.set('n', '<leader>bn', '<cmd>bnext<CR>', { desc = '[N]ext buffer' })
vim.keymap.set('n', '<leader>bp', '<cmd>bprev<CR>', { desc = '[P]revious buffer' })
vim.keymap.set('n', '<leader>bk', '<cmd>bdelete<CR>', { desc = '[K]ill buffer' })

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Move in visual lines with j and k
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')

-- Netrw
vim.keymap.set('n', '<leader>oe', '<cmd>Explore<CR>', { desc = '[E]xplore' })

-- Quickfix
vim.keymap.set('n', '<leader>q', '<cmd>copen<CR>', { desc = '[Q]uickfixlist' })
vim.keymap.set('n', '<C-n>', '<cmd>cn<CR>', { desc = '[Q]uickfix [N]ext' })
vim.keymap.set('n', '<C-p>', '<cmd>cp<CR>', { desc = '[Q]uickfix [P]revious' })
