return {
  "folke/snacks.nvim",
  opts = {
    scroll = { enabled = false },
    indent = {
      animate = { enabled = false },
    },
    picker = {
      sources = {
        explorer = {
          auto_close = true,
        },
        files = {
          hidden = true,
        },
        grep = {
          hidden = true,
        },
      },
    },
    image = {
      img_dirs = { "img", "images", "assets", "static", "public", "media", "attachments", "Attachments" },
      doc = {
        inline = true,
        enabled = true,
        max_width = 80,
        max_height = 20,
      },
      math = {
        enabled = true,
      },
    },
  },
}
