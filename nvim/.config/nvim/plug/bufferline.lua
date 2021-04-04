--  _            __  __           _ _
-- | |__  _   _ / _|/ _| ___ _ __| (_)_ __   ___
-- | '_ \| | | | |_| |_ / _ \ '__| | | '_ \ / _ \
-- | |_) | |_| |  _|  _|  __/ |  | | | | | |  __/
-- |_.__/ \__,_|_| |_|  \___|_|  |_|_|_| |_|\___|
--
--


require('bufferline').setup{
   options = {
     view = "default",
     separator_style = "thin",
     buffer_close_icon = ' ',

     -- Enabling lsp diagnostics
     diagnostics = "nvim_lsp",

     -- Diagnostics icons
     diagnostics_indicator = function(count, level)
        local icon = level:match("error") and " " or " "
        return " "..icon..count
      end
   }
}
