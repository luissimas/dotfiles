;;; init-defaults.el --- Sane defaults. -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration to provide better default behaviour to Emacs.

;;; Code:
(require 'pada-utils)

;; Handling backups and autosaves
(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves" user-emacs-directory) t))
      backup-directory-alist `(("." . ,(expand-file-name "tmp/backups" user-emacs-directory)))
      create-lockfiles nil)

;; Stop asking for following git symlink
(setq vc-follow-symlinks t)

;; Prompts and confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable suggestion for keybindings in minibuffer
(setq suggest-key-bindings nil)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disabling init screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Disabling main UI components
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq visible-bell nil
      use-dialog-box nil)
(setq-default cursor-in-non-selected-windows nil)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Fringes
(set-fringe-mode '(5 . 0))

;; Cursor offset
(setq scroll-margin 8
      scroll-conservatively 101)

;; Remember cursor position
(save-place-mode 1)

;; One space is enough to end a sentence
(setq sentence-end-double-space nil)

;; Remove truncation and continuation indicators
(setq-default fringe-indicator-alist
              (assq-delete-all 'continuation
                               (assq-delete-all 'truncation fringe-indicator-alist)))

;; Spaces over tabs
(setq standard-indent 2)
(setq backward-delete-char-untabify-method 'hungry)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable line numbers and truncate lines only on programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (setq display-line-numbers 'relative)
                            (toggle-truncate-lines 1)))

;; Unique buffer name formats
(setq uniquify-buffer-name-style 'forward)

;; Autopairs
(electric-pair-mode)

;; Pretty symbols
(global-prettify-symbols-mode)

;; Font configuration
(defvar pada/default-font-size (if (pada/is-laptop) 100 100))
(defvar pada/default-font-family "Iosevka Padawan")

(defvar pada/variable-font-size (if (pada/is-laptop) 1.1 1.1))
(defvar pada/variable-font-family "Fira Sans")

(defun pada/set-fonts ()
  "Set the main font faces."
  (interactive)
  (set-face-attribute 'default nil :font pada/default-font-family :height pada/default-font-size :weight 'normal)
  (set-face-attribute 'fixed-pitch nil :font pada/default-font-family :height 1.0 :weight 'normal)
  (set-face-attribute 'variable-pitch nil :font pada/variable-font-family :height pada/variable-font-size :weight 'light))

;; Enable frame pixelwise resizing
(setq frame-resize-pixelwise t)

(defun pada/set-frame-parameters ()
  "Set custom parameters for the current frame."
  (interactive)
  (dolist (parameter '((no-special-glyphs t)
                       (internal-border-width 0)))
    (set-frame-parameter (selected-frame) (car parameter) (car (cdr parameter)))))

;; Setting frame options in both daemon (with hooks) or on
;; normal emacs startup (directly calling the functions)
(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook 'pada/set-fonts)
      (add-hook 'server-after-make-frame-hook 'pada/set-frame-parameters))
  (progn
    (pada/set-fonts)
    (pada/set-frame-parameters)))

;; Delete frames
(setq frame-auto-hide-function 'delete-frame)

;; Make new buffers reuse the same window
(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))

;; Avoid automatic window resizing
(customize-set-variable 'even-window-sizes nil)

;; Always kill the buffer when quitting a window
(global-set-key [remap quit-window] #'(lambda () (interactive) (quit-window t)))
(global-set-key [remap magit-mode-bury-buffer] #'(lambda () (interactive) (magit-mode-bury-buffer t)))

;; Buffer display rules
;; NOTE: I really need to understand all of this better
(setq display-buffer-alist
      '(("\\`\\*Calendar\\*\\'"
         (display-buffer-below-selected))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|Messages\\|Async Shell Command\\|Python\\|prolog\\|SQL:.*\\|exunit-compilation\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . 0))
        ("\\*\\(lsp-help\\|lsp-documentation\\|eldoc\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom))
        ("\\*\\([Hh]elp.*\\|info\\)\\*"
         (display-buffer-in-side-window)
         (window-width . 0.4)
         (side . right)
         (slot . 0))
        ("\\*\\(.*e?shell\\|.*vterm\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . -1))
        ("\\*Personal Finance\\*"
         (display-buffer-in-side-window)
         (window-width . 0.4)
         (side . right)
         (slot . 0))))

;; Default compile command
;; TODO: make it major-mode dependent
(setq compile-command "yarn dev")

;; Colorize compilation buffers
(defun pada/colorize-compilation-buffer ()
  "Colorize compilation-buffer using `ansi-color'."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'pada/colorize-compilation-buffer)

(provide 'init-defaults)
;;; init-defaults.el ends here
