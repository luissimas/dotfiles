-- blink.cmp source that completes hledger account names on posting lines.
-- Accounts are read from the buffer itself, so new or unsaved accounts show up
-- right away and half-written transactions don't break completion.
local source = {}

function source.new()
  return setmetatable({}, { __index = source })
end

function source:enabled()
  return vim.bo.filetype == "ledger"
end

function source:get_trigger_characters()
  return { ":" }
end

-- The account is the text after the indent, up to two spaces, a tab or the end of the line
local function posting_account(line)
  return line:match("^%s+([^;%s].-)%s%s") or line:match("^%s+([^;%s].-)\t") or line:match("^%s+([^;%s].-)%s*$")
end

local function buffer_accounts(bufnr, skip_lnum)
  local seen, accounts = {}, {}
  for lnum, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
    local account = lnum ~= skip_lnum and (line:match("^account%s+(.-)%s*$") or posting_account(line))
    if account and not seen[account] then
      seen[account] = true
      table.insert(accounts, account)
    end
  end
  return accounts
end

-- Each typed segment must prefix an account segment, in order, but account
-- segments can be skipped. So "ex:tr:eat" matches "Expenses:Travel:Eating Out"
-- and "ac:ubs" matches "Assets:Accounts:UBS"
local function matches(account, typed)
  local account_parts = vim.split(account:lower(), ":")
  local i = 1
  for _, part in ipairs(vim.split(typed:lower(), ":")) do
    while i <= #account_parts and not vim.startswith(account_parts[i], part) do
      i = i + 1
    end
    if i > #account_parts then
      return false
    end
    i = i + 1
  end
  return true
end

function source:get_completions(ctx, callback)
  local indent, typed = ctx.line:sub(1, ctx.cursor[2]):match("^(%s+)(.*)$")
  -- Only complete the account part of a posting, not the amount or a comment
  if not indent or vim.startswith(typed, ";") or typed:find("%s%s") or typed:find("\t") then
    callback({ items = {} })
    return
  end

  local range = {
    start = { line = ctx.cursor[1] - 1, character = #indent },
    ["end"] = { line = ctx.cursor[1] - 1, character = ctx.cursor[2] },
  }
  local items = {}
  for _, account in ipairs(buffer_accounts(ctx.bufnr, ctx.cursor[1])) do
    if matches(account, typed) then
      table.insert(items, {
        label = account,
        filterText = account,
        kind = require("blink.cmp.types").CompletionItemKind.Property,
        textEdit = { newText = account, range = range },
      })
    end
  end
  -- Ask blink to call us on every keystroke, so the segment matching above
  -- applies instead of blink's fuzzy matching on the last word only
  callback({ items = items, is_incomplete_forward = true, is_incomplete_backward = true })
end

return source
