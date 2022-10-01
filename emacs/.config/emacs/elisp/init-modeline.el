;;; init-modeline.el --- Modeline configuration . -*- lexical-binding: t -*-
;;; Commentary:

;; Modeline configuration.

;;; Code:

;; Evil mode indication position
;; (setq evil-mode-line-format '(before . mode-line-front-space))

;; Macro recording display format
(setq mode-line-defining-kbd-macro
      (propertize " Recording macro..." 'face 'mode-line-emphasis))

(defun pada/replace-vc-string (vc-string)
  "Replace VC-STRING with a simpler and more pleasent representation.
This function is meant to advise `vc-git-mode-line-string', particularly
as a `:filter-result' advice."
  (replace-regexp-in-string ".*Git[:-]" "" vc-string))

(advice-add 'vc-git-mode-line-string :filter-return 'pada/replace-vc-string)

(setq column-number-mode 1
      line-number-mode 1)

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                "    "
                mode-line-position
                "    "
                (vc-mode vc-mode)
                "    "
                mode-line-modes
                "    "
                mode-line-misc-info
                mode-line-end-spaces))

(use-package minions
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '("" . ""))
  (minions-prominent-modes '(defining-kbd-macro))
  :init
  (minions-mode))

;; Time display format
(setq display-time-format "%A %d %b, %H:%M")
(setq display-time-default-load-average nil)

;; Hide modeline
(use-package hide-mode-line
  :hook ((vterm-mode compilation-mode treemacs-mode shell-mode) . hide-mode-line-mode))

(provide 'init-modeline)
;;; Code:
;;; init-modeline.el ends here
