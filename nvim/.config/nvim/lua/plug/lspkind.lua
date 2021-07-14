require("lspkind").init(
  {
    -- enables text annotations
    with_text = true,
    preset = "default", -- default | codicons
    symbol_map = {
      Text = "",
      Method = "ƒ",
      Function = "",
      Constructor = "",
      Variable = "",
      Class = "",
      Interface = "ﰮ",
      Module = "",
      Property = "",
      Unit = "",
      Value = "",
      Enum = "了",
      Keyword = "",
      Snippet = "﬌",
      Color = "",
      File = "",
      Folder = "",
      EnumMember = "",
      Constant = "",
      Struct = ""
    }
  }
)

-- Enabling lspkind icons
require("lspkind").init()
