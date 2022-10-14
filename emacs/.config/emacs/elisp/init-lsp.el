;;; init-lsp.el --- LSP configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; LSP configuration.

;;; Code:

(defun pada/lsp-consult-xref-setup ()
  "Setup xref to use consult functions."
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(defun pada/lsp-corfu-setup ()
  "Setup corfu completion style for lsp."
  (setq-local completion-styles '(orderless basic)
              completion-category-defaults nil))

;; (use-package lsp-mode
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   (setq lsp-completion-provider :none)
;;   :hook
;;   ((js-mode typescript-mode tuareg-mode c-mode python-mode elixir-mode web-mode css-mode) . lsp)
;;   (lsp-mode . lsp-enable-which-key-integration)
;;   (lsp-mode . pada/lsp-consult-xref-setup)
;;   (lsp-mode . pada/lsp-corfu-setup)
;;   :config
;;   (setq read-process-output-max (* 1024 1024)
;;         lsp-headerline-breadcrumb-enable nil
;;         lsp-modeline-code-actions-enable t
;;         lsp-modeline-diagnostics-enable t
;;         lsp-enable-snippet t
;;         lsp-signature-doc-lines 1
;;         lsp-auto-guess-root t
;;         lsp-enable-on-type-formatting nil
;;         lsp-signature-render-documentation t
;;         lsp-elixir-suggest-specs nil
;;         lsp-log-io nil
;;         lsp-restart 'iteractive)
;;   (general-define-key :states 'normal "gr" 'lsp-find-references)
;;   (pada/leader-key
;;     "l" '(:ignore t :which-key "LSP")
;;     "lf" '(lsp-format-buffer :which-key "Format buffer")
;;     "la" '(lsp-execute-code-action :which-key "Code actions")
;;     "lh" '(lsp-describe-thing-at-point :which-key "Describe symbol at point")
;;     "li" '(lsp-organize-imports :which-key "Organize imports")
;;     "ld" '(consult-lsp-diagnostics :which-key "Diagnostics")
;;     "lr" '(lsp-rename :which-key "Rename"))
;;   ;; Disable creation on ts-server .log files
;;   (setq lsp-clients-typescript-server-args '("--stdio"))
;;   :commands lsp)

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil
;;         lsp-ui-doc-header nil
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-delay 0
;;         lsp-ui-doc-max-height 50
;;         lsp-ui-doc-position 'at-point
;;         lsp-ui-peek-enable nil
;;         lsp-ui-imenu-enable nil
;;         lsp-ui-sideline-enable nil))

;; (use-package consult-lsp
;;   :after lsp
;;   :config
;;   (pada/leader-key
;;     "ld" '(consult-lsp-diagnostics :which-key "Diagnostics")))

;; (use-package lsp-pyright
;;   :after lsp-mode)

;; (use-package flycheck
;;   :hook
;;   (prog-mode . flycheck-mode)
;;   (flycheck-mode . flycheck-set-indication-mode)
;;   :custom
;;   (flycheck-display-errors-delay 0.6)
;;   (flycheck-idle-change-delay 0.01)
;;   (flycheck-check-syntax-automatically '(save idle-buffer-switch  idle-change mode-enabled))
;;   (flycheck-indication-mode 'left-margin))

(use-package eglot
  :hook
  ((js-mode typescript-mode tuareg-mode c-mode python-mode elixir-mode web-mode css-mode) . eglot-ensure)
  :config
  (defun pada/setup-eglot-eldoc ()
    (push 'flymake-eldoc-function eldoc-documentation-functions))
  (add-hook 'eglot-managed-mode-hook #'pada/setup-eglot-eldoc)

  (defun pada/consult-flymake-project ()
    (interactive)
    (consult-flymake t))

  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(elixir-mode . ("elixir-ls")))

  (pada/leader-key
    :keymaps 'eglot-mode-map
    "l" '(:ignore t :which-key "LSP")
    "lf" '(eglot-format :which-key "Format buffer")
    "la" '(eglot-code-actions :which-key "Code actions")
    "ld" '(pada/consult-flymake-project :which-key "Diagnostics")
    "ls" '(consult-eglot-symbols :which-key "Find symbol")
    "lh" '(eldoc :which-key "Describe symbol at point")
    "li" '(eglot-code-action-organize-imports :which-key "Organize imports")
    "lr" '(eglot-rename :which-key "Rename")))

(use-package consult-eglot)

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer nil)
  (eldoc-current-idle-delay 0.2)
  (eldoc-documentation-strategy 'eldoc-documentation-compose))

(provide 'init-lsp)
;;; Code:
;;; init-lsp.el ends here