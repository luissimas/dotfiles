;;; init-format.el --- Formatting configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Formating configuration.

;;; Code:

(use-package apheleia
  :init
  (apheleia-global-mode))

(use-package editorconfig
  :config
  (editorconfig-mode))

(provide 'init-format)
;;; Code:
;;; init-format.el ends here
