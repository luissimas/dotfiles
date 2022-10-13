;;; init-highlight.el --- Highlights configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Highlight-related packages configuration.

;;; Code:

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (css-mode web-mode))

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       font-lock-constant-face bold)
          ("WARNING"    warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold))))

(provide 'init-highlight)
;;; init-highlight.el ends here
