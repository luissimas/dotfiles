;;; init-elixir.el --- Elixir language support. -*- lexical-binding: t -*-
;;; Commentary:

;; Elixir language suport.

;;; Code:

(use-package elixir-mode)

(use-package exunit
  :hook (elixir-mode . exunit-mode))

(use-package flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (flycheck-credo-setup))

(provide 'init-elixir)
;;; Code:
;;; init-elixir.el ends here
