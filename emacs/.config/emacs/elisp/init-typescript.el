;;; init-typescript.el --- Typescript language support. -*- lexical-binding: t -*-
;;; Commentary:

;; Typescript language suport.

;;; Code:

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-mode-hook . (lambda () (add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)))
  :custom
  (js-indent-level 2)
  (typescript-indent-level 2))

;; Web-mode for all the HTML/CSS/JSX/TSX stuff
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-quoting nil
        web-mode-enable-comment-keywords t)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

;; Tailwindcss
(use-package lsp-tailwindcss
  :straight '(:type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(provide 'init-typescript)
;;; Code:
;;; init-typescript.el ends here
