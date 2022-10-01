;;; init-text-scale.el --- Text scaling. -*- lexical-binding: t -*-
;;; Commentary:

;; Text scaling configuration.

;;; Code:

(setq-default text-scale-mode-amount 3)

(use-package default-text-scale
  :config
  (setq default-text-scale-amount 10)
  (general-define-key
   :keymaps 'default-text-scale-mode-map
   "C-="  'default-text-scale-increase
   "C--"  'default-text-scale-decrease)
  :init
  (default-text-scale-mode))

(provide 'init-text-scale)
;;; Code:
;;; init-text-scale.el ends here
