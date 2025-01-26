return {
  'saghen/blink.cmp',
  dependencies = {
    'rafamadriz/friendly-snippets',
    { 'saghen/blink.compat', lazy = true, verson = false },
  },
  version = '*',
  opts = {
    appearance = {
      use_nvim_cmp_as_default = true,
      nerd_font_variant = 'mono',
    },
    sources = {
      default = { 'lsp', 'path', 'snippets', 'buffer', 'obsidian', 'obsidian_new', 'obsidian_tags' },
      cmdline = {},
      providers = {
        obsidian = { name = 'obsidian', module = 'blink.compat.source' },
        obsidian_new = { name = 'obsidian_new', module = 'blink.compat.source' },
        obsidian_tags = { name = 'obsidian_tags', module = 'blink.compat.source' },
      },
    },
    completion = {
      accept = { auto_brackets = { enabled = false } },
      menu = {
        scrollbar = false,
        draw = {
          columns = { { 'kind_icon' }, { 'label', 'label_description', gap = 1 } },
        },
      },
      documentation = { auto_show = true, auto_show_delay_ms = 500 },
    },
    signature = { enabled = true },

    keymap = {
      preset = 'default',
      ['<C-u>'] = { 'scroll_documentation_up', 'fallback' },
      ['<C-d>'] = { 'scroll_documentation_down', 'fallback' },
      ['<CR>'] = { 'accept', 'fallback' },
    },
  },
  opts_extend = { 'sources.default' },
}
