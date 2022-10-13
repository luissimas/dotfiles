;;; init-elixir.el --- Elixir language support. -*- lexical-binding: t -*-
;;; Commentary:

;; Elixir language suport.

;;; Code:

(use-package elixir-mode
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (push '("|>" . ?\u25B7) prettify-symbols-alist))))

(use-package exunit
  :hook elixir-mode)

(use-package inf-elixir)

(use-package flycheck-credo
  :after elixir-mode
  :config
  (setq flycheck-elixir-credo-strict t)
  (flycheck-credo-setup))

(provide 'init-elixir)
;;; Code:
;;; init-elixir.el ends here
