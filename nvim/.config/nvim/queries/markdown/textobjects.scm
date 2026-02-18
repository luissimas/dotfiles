; extends

(fenced_code_block) @code_block.outer

(fenced_code_block
  (fenced_code_block_delimiter) @_start
  (code_fence_content) @code_block.inner
  (fenced_code_block_delimiter) @_end)
