;;; init-format.el --- Formatting configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Formating configuration.

;;; Code:

(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  :config
  (setq format-all-show-errors 'never))

(use-package editorconfig
  :config
  (editorconfig-mode))

(provide 'init-format)
;;; Code:
;;; init-format.el ends here
