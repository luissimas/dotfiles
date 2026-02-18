---@alias obsidian.PartialUI obsidian.config.UIOpts | {}

local zettelkasten_root = "~/projects/zettelkasten/"

return {
  "obsidian-nvim/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  ft = "markdown",
  dependencies = {
    -- Required.
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "nvim-treesitter/nvim-treesitter",
  },
  ---@type obsidian.config
  opts = {
    legacy_commands = false,
    workspaces = {
      {
        name = "personal",
        path = zettelkasten_root,
      },
    },
    templates = {
      folder = "Tools/Templates",
      date_format = "%Y-%m-%d",
      time_format = "%H:%M",
      substitutions = {},
    },
    daily_notes = {
      folder = "4. Journal",
      date_format = "%Y-%m-%d",
    },
    attachments = {
      folder = "Attachments",
      img_text_func = function(path)
        path = path.vault_relative_path() or path
        return string.format("![[%s]]", path)
      end,
    },
    ---@type obsidian.PartialUI
    ui = {
      enable = true,
    },
    -- Customize how note IDs are generated given an optional title.
    ---@param title string|?
    ---@return string
    note_id_func = function(title)
      if title ~= nil then
        return title
      else
        return tostring(os.time())
      end
    end,
    -- Customize how note file names are generated given the ID, target directory, and title.
    ---@param spec { id: string, dir: obsidian.Path, title: string|? }
    ---@return string|obsidian.Path The full path to the new note.
    note_path_func = function(spec)
      local path = require("obsidian.path")
      local note_path = path.new(zettelkasten_root) / "Inbox" / tostring(spec.id)
      return note_path:with_suffix(".md")
    end,
  },
  keys = {
    { "<leader>fn", "<cmd>Obsidian quick_switch<CR>", desc = "[F]ind [N]otes" },
    { "<leader>ob", "<cmd>Obsidian backlinks<CR>", desc = "[O]pen [B]acklinks" },
    { "<leader>oo", "<cmd>Obsidian open<CR>", desc = "[O]pen [O]bsidian" },
    { "<leader>op", "<cmd>Obsidian paste_img<CR>", desc = "[O]bsidian [P]aste Image" },
    { "<leader>or", "<cmd>Obsidian rename<CR>", desc = "[O]bsidian [R]ename Note" },
    { "<leader>on", "<cmd>Obsidian new<CR>", desc = "[N]ew Note" },
    { "<leader>og", "<cmd>Obsidian search<CR>", desc = "[G]rep" },
    { "<leader>ot", "<cmd>Obsidian template<CR>", desc = "Insert [T]emplate" },
    { "<leader>od", "<cmd>Obsidian dailies<CR>", desc = "Open [D]ailies" },
    {
      "<CR>",
      function()
        return require("obsidian").util.smart_action()
      end,
      desc = "Follow Link",
      mode = "n",
      ft = "markdown",
    },
  },
}
