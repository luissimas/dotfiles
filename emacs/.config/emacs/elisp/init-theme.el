;;; init-theme.el --- Theme configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Theme configuration.

;;; Code:
(require 'pada-utils)

(defcustom pada/system-theme-associations
  '(("modus-operandi" modus-operandi)
    ("modus-vivendi" modus-vivendi)
    ("nord" doom-nord-aurora)
    ("gruvbox" doom-gruvbox)
    ("tokyonight" doom-tokyo-night)
    ("palenight" doom-palenight)
    ("pywal" ewal-doom-one)
    ("catppuccin" catppuccin))
  "A alist of association between file patterns and external programs."
  :group 'system-theme
  :type "alist")

(defun pada/load-system-theme ()
  "Read file ~/.colorscheme and load its theme."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.colorscheme")
    (let ((theme (string-trim (buffer-string)))
          (associations pada/system-theme-associations))
      (while associations
        (let* ((current (pop associations))
               (system-theme (car current))
               (emacs-theme (car (cdr current))))
          (when (string-match-p system-theme theme)
            (pada/load-theme emacs-theme)
            (setq associations nil)))))))

;; Load system theme on startup
(add-hook 'emacs-startup-hook 'pada/load-system-theme)

;; Themes
(use-package modus-themes
  :straight nil
  :init
  (setq modus-themes-subtle-line-numbers t
        modus-themes-mode-line '(borderless)
        modus-themes-org-blocks 'gray-background
        modus-themes-hl-line '(accented)))

(use-package nano-theme)

(use-package doom-themes
  :custom
  (doom-gruvbox-dark-variant "hard"))

(use-package mindre-theme
  :straight '(:host github :repo "erikbackman/mindre-theme")
  :config
  (setq mindre-use-more-bold nil
        mindre-use-faded-lisp-parens nil))

;; Pywal
(use-package ewal)

(use-package ewal-spacemacs-themes
  :config
  (setq spacemacs-theme-underline-parens nil))

(use-package ewal-doom-themes)

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(provide 'init-theme)
;;; init-theme.el ends here
