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

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(provide 'init-typescript)
;;; Code:
;;; init-typescript.el ends here
