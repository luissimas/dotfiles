;;; init-completion.el --- Completion configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Completion framework configuration.

;;; Code:

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :custom
  (vertico-cycle t)
  :config
  (general-define-key
   :states '(normal insert)
   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   (general-define-key
    :states 'normal
    :keymaps 'vertico-map
    "<escape>" 'abort-minibuffers)
   :init
   (vertico-mode)))

;; Persist history over Emacs restarts. Vertico and corfu sort by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package vetico-repeat
  :after vertico
  :straight nil
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-directory
  :after vertico
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(defun pada/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher for strings using the equal sign (`=') as a suffix."
  (when (string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1))))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-ignore-case t
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
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

(defun pada/corfu-quit ()
  "Quits corfu completion and enter evil normal mode."
  (interactive)
  (corfu-quit)
  (evil-normal-state))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 1)
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first t)
  (corfu-bar-width 0)
  (corfu-min-width 20)
  (corfu-max-width 100)
  (corfu-echo-documentation 0.2) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  :config
  (general-define-key
   :states 'insert
   "C-SPC" #'completion-at-point)
  (general-define-key
   :states 'insert
   :keymaps 'corfu-map
   [escape] #'pada/corfu-quit
   "ESC" #'pada/corfu-quit
   "C-SPC" #'corfu-quit
   "C-h" #'corfu-info-documentation)
  :init
  (global-corfu-mode))

(use-package corfu-history
  :after corfu
  :straight nil
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history)
  :init
  (corfu-history-mode))

(use-package corfu-info
  :after corfu
  :straight nil)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell))

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(provide 'init-completion)
;;; Code:
;;; init-completion.el ends here
