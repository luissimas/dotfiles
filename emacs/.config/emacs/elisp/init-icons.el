;;; init-icons.el --- Icons configuration . -*- lexical-binding: t -*-
;;; Commentary:

;; Icons configuration.

;;; Code:

(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

(use-package all-the-icons-dired
  :requires all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-icons)
;;; Code:
;;; init-icons.el ends here
