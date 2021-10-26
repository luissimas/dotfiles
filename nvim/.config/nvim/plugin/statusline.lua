-- Statusline
function _G.gitbranch()
  local branch = vim.fn.system(
    "git -C "
    .. vim.fn.expand('%:h') ..
    " branch --show-current 2>/dev/null"
  ):gsub("\n", "")

  if branch ~= "" then
    return string.format("[%s] ", branch)
  end

  return ""
end

-- vim.o.statusline = " %{v:lua.gitbranch()}%f %m %r %= %y %p%% "
vim.o.statusline = " %f %m %r %= %y %p%% "
