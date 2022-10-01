;;; init-undo.el --- undo-tree configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; undo-tree configuration.

;;; Code:

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  (global-undo-tree-mode))

(provide 'init-undo)
;;; init-undo.el ends here
