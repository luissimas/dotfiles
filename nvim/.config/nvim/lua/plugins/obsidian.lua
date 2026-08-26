local zettelkasten_root = "~/projects/zettelkasten/"
local inbox_directory = "Inbox"
local journal_directory = "Journal"
local daily_directory = journal_directory .. "/Daily"
local weekly_directory = journal_directory .. "/Weekly"
local weekly_date_format = "GGGG-[W]WW"

-- Open the current week's note, creating it from the template if needed.
local function open_weekly_note()
  local Note = require("obsidian.note")
  local Path = require("obsidian.path")
  local util = require("obsidian.util")

  local id = tostring(util.format_date(os.time(), weekly_date_format))
  local note_path = Path.new(Obsidian.dir) / weekly_directory / (id .. ".md")

  local note
  if note_path:is_file() then
    note = Note.from_file(note_path)
  else
    note = Note.create({
      id = id,
      verbatim = true,
      dir = note_path:parent(),
      template = "Weekly.md",
    })
  end

  if not note:exists() then
    note:write()
  end
  note:open()
end

return {
  "obsidian-nvim/obsidian.nvim",
  version = "*", -- recommended, use latest release instead of latest commit
  event = {
    "BufReadPre " .. vim.fn.expand(zettelkasten_root) .. "/**/*.md",
    "BufNewFile " .. vim.fn.expand(zettelkasten_root) .. "/**/*.md",
  },
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
      substitutions = {
        yesterday = function()
          return os.date("%Y-%m-%d", os.time() - 86400)
        end,
        tomorrow = function()
          return os.date("%Y-%m-%d", os.time() + 86400)
        end,
      },
    },
    daily_notes = {
      folder = daily_directory,
      template = "Daily.md",
      date_format = "%Y-%m-%d",
    },
    frontmatter = {
      enabled = true,
      func = function(note)
        local out = { id = note.id, aliases = note.aliases, tags = note.tags }

        -- Preserve existing metadata
        if note.metadata ~= nil and not vim.tbl_isempty(note.metadata) then
          for k, v in pairs(note.metadata) do
            out[k] = v
          end
        end

        if out["created-at"] == nil then
          out["created-at"] = os.date("%Y-%m-%d")
        end

        out["updated-at"] = os.date("%Y-%m-%d")

        return out
      end,
      sort = { "id", "created-at", "updated-at", "aliases", "tags" },
    },
    attachments = {
      folder = "Attachments",
      img_text_func = function(path)
        path = path.vault_relative_path() or path
        return string.format("![[%s]]", path)
      end,
    },
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

      local journal = path.new(vim.fs.joinpath(vim.fn.expand(zettelkasten_root), journal_directory))

      -- Keep the requested directory for journal notes (daily and weekly)
      if spec.dir ~= nil and (spec.dir == journal or journal:is_parent_of(spec.dir)) then
        return (spec.dir / tostring(spec.id)):with_suffix(".md")
      end

      return (path.new(zettelkasten_root) / path.new(inbox_directory) / tostring(spec.id)):with_suffix(".md")
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
    { "<leader>ow", open_weekly_note, desc = "Open [W]eekly" },
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
