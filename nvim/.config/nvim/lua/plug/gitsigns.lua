require("gitsigns").setup {
  signs = {
    add = {hl = "GitSignsAdd", text = "│", numhl = "GitSignsAddNr", linehl = "GitSignsAddLn"},
    change = {hl = "GitSignsChange", text = "│", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn"},
    delete = {hl = "GitSignsDelete", text = "_", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn"},
    topdelete = {hl = "GitSignsDelete", text = "‾", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn"},
    changedelete = {hl = "GitSignsChange", text = "~", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn"}
  },
  numhl = false,
  linehl = false,
  keymaps = {
    noremap = true,
    buffer = true,
    ["n B"] = '<cmd>lua require"gitsigns".blame_line()<CR>'
  },
  watch_index = {
    interval = 1000
  },
  current_line_blame = false,
  sign_priority = 6,
  signcolumn = true,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  use_decoration_api = true,
  use_internal_diff = true, -- If luajit is present
  current_line_blame_formatter = function(name, blame_info)
    if blame_info.author == name then
      blame_info.author = "You"
    end

    local text

    if blame_info.author == "Not Committed Yet" then
      text = blame_info.author
    else
      text =
        string.format(
        "%s, %s - %s",
        blame_info.author,
        os.date("%d/%m/%Y", tonumber(blame_info["author_time"])),
        blame_info.summary
      )
    end

    return {{" " .. text, "GitSignsCurrentLineBlame"}}
  end
}
