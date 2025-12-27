return {
  "L3MON4D3/LuaSnip",
  config = function()
    local ls = require("luasnip")
    local s = ls.snippet
    local t = ls.text_node
    local i = ls.insert_node
    local f = ls.function_node

    -- Get today's date in YYYY/MM/DD format
    local function today()
      return os.date("%Y/%m/%d")
    end

    -- Add hledger snippets
    ls.add_snippets("ledger", {
      s("t", {
        f(today, {}),
        t(" * "),
        i(1, "Description"),
        t({ "", "    " }),
        i(2, "Expenses:Category"),
        t("                        CHF"),
        i(3, "0.00"),
        t({ "", "    " }),
        i(4, "Assets:Account"),
        i(0),
      }),
    })
  end,
}
