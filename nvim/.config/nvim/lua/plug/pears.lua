require("pears").setup(function(conf)
  conf.expand_on_enter(true)

  -- HTML tag matching and stuff
  conf.preset("tag_matching")

  -- Making it work properly with compe
  conf.on_enter(function(pears_handle)
    if vim.fn.pumvisible() == 1 and vim.fn.complete_info().selected ~= -1 then
      return vim.fn["compe#confirm"]("<CR>")
    else
      pears_handle()
    end
  end)
end)
