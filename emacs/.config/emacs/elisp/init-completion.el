;;; init-completion.el --- Completion configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Completion framework configuration.

;;; Code:

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  ;; Using vertico-directory extension
  (add-to-list 'load-path (expand-file-name "straight/build/vertico/extensions" user-emacs-directory))
  (require 'vertico-directory)
  (general-define-key
   :states '(normal insert)
   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   "RET" 'vertico-directory-enter
   "DEL" 'vertico-directory-delete-char)
  (general-define-key
   :states 'normal
   :keymaps 'vertico-map
   "<escape>" 'abort-minibuffers)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(defun pada/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher for strings using the equal sign (`=') as a suffix."
  (when (string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1))))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)
        orderless-style-dispatchers '(pada/orderless-literal-dispatcher)))

;; Richer completion annotations
(use-package all-the-icons-completion
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line))
  :config
  (setq consult-narrow-key (kbd "C-.")
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package consult-lsp
  :after lsp
  :config
  (pada/leader-key
    "ld" '(consult-lsp-diagnostics :which-key "Diagnostics")))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable search and replace in embark buffers
(use-package wgrep)

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-maximum-width 120)
  (company-tooltip-minimum-width 60)
  (company-tooltip-align-annotations t)
  :config
  ;; Unbinding default insert mappings
  (general-define-key
   :states 'insert
   "C-j" nil
   "C-k" nil)
  (general-define-key
   :states 'insert
   :keymaps 'company-active-map
   "C-j"  'company-select-next
   "C-k"  'company-select-previous)
  (general-define-key
   :states 'insert
   :keymaps 'company-mode-map
   "C-SPC"  'company-complete))

(use-package company-box
  :custom
  (company-box-scrollbar nil)
  (company-box-doc-enable nil)
  :hook
  (company-mode . company-box-mode))

(provide 'init-completion)
;;; Code:
;;; init-completion.el ends here
