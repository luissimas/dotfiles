;;; init-yas.el --- Yasnippet configuration . -*- lexical-binding: t -*-
;;; Commentary:

;; Yasnippet configuration.

;;; Code:

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package consult-yasnippet)

(provide 'init-yas)
;;; Code:
;;; init-yas.el ends here