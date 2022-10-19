;;; init-typescript.el --- Typescript language support. -*- lexical-binding: t -*-
;;; Commentary:

;; Typescript language suport.

;;; Code:

(use-package typescript-mode
  ;; :hook
  ;; (typescript-mode-hook . (lambda () (add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes)))
  :custom
  (js-indent-level 2)
  (typescript-indent-level 2)
  :config
  ;; Deriving typescript-mode to work with tsx files
  ;; see: https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
  (define-derived-mode typescriptreact-mode typescript-mode "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; Web-mode for all the HTML/CSS/JSX/TSX stuff
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ;; ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.[hl]?eex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-quoting nil
        web-mode-enable-comment-keywords t))

;; Tailwindcss
(use-package lsp-tailwindcss
  :straight '(:type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(provide 'init-typescript)
;;; Code:
;;; init-typescript.el ends here
