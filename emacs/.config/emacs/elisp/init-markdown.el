;;; init-markdown.el --- Markdown configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Markdown configuration.

;;; Code:

(use-package markdown-mode
  :hook
  (markdown-mode . flyspell-mode)
  :init
  (setq-default markdown-hide-markup t
                markdown-enable-wiki-links t
                markdown-enable-math t
                markdown-wiki-link-alias-first nil
                markdown-wiki-link-search-subdirectories t
                markdown-wiki-link-search-parent-directories t
                markdown-link-space-sub-char " "))

;; Display inline latex formulas and images
(use-package texfrag
  :hook
  (texfrag-mode . texfrag-document)
  (markdown-mode . texfrag-mode)
  (latex-mode . texfrag-mode))

(provide 'init-markdown)
;;; Code:
;;; init-markdown.el ends here
