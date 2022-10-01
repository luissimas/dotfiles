;;; init-spell.el --- Spell checking configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Spell checking configuration.

;;; Code:

(with-eval-after-load "ispell"
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "pt_BR,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pt_BR,en_US"))

(use-package flyspell-correct
  :config
  (general-define-key :states 'insert :keymaps 'flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper))

(provide 'init-spell)
;;; Code:
;;; init-spell.el ends here
