;;; init-which-key.el --- which-key configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; which-key configuration.

;;; Code:

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 5)
  (which-key-mode))

(provide 'init-which-key)
;;; init-which-key.el ends here
