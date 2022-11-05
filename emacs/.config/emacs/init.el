;;; init.el --- Emacs configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Main config entry point.

;;; Code:

;; Increase garbage collection on startup and decrease it thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Profile startup time
(defun pada/display-startup-time ()
  "Display the startup time for the current section."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'pada/display-startup-time)

;; Set the correct native compilation path
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Supress native compilation warnings
(setq native-comp-async-report-warnings-errors nil
      warning-minimum-level :emergency)

;; Set custom file
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Set modules path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Straight bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)


;;;-------------------------------------------------------------------------------
;;; Modules
;; Base
(require 'init-defaults)
(require 'init-modeline)
(require 'init-theme)
(require 'init-open-external)
(require 'init-path)
(require 'init-persp)
(require 'init-helpful)

;; Keybindings
(require 'init-general)
(require 'init-evil)
(require 'init-undo)
(require 'init-which-key)

;; Development
(require 'init-git)
(require 'init-highlight)
(require 'init-project)
(require 'init-format)
(require 'init-term)
(require 'init-completion)
(require 'init-lsp)
(require 'init-yas)

;; UI
(require 'init-icons)
(require 'init-ligature)

;; Language support
(require 'init-typescript)
(require 'init-ocaml)
(require 'init-elixir)
(require 'init-org)
(require 'init-markdown)

;; Misc
(require 'init-text-scale)
(require 'init-spell)
(require 'init-ledger)
(require 'init-mpdel)
(require 'init-screenshot)
(require 'init-docker)
;;-------------------------------------------------------------------------------


;; Load custom file
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
