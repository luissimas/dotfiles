;;; init-evil.el --- Evil configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Evil configuration.

;;; Code:

(defun pada/evil-lookup-func ()
  "Lookup contex-aware documentation for symbols.
This function is meant to be used by `evil-lookup'."
  (cond
   ((and (boundp 'lsp-mode) lsp-mode) (lsp-ui-doc-glance))
   ((equal major-mode #'emacs-lisp-mode) (helpful-at-point))
   (t (dictionary-lookup-definition))))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-minibuffer t
        evil-undo-system 'undo-tree
        evil-want-Y-yank-to-eol t
        evil-shift-width tab-width)
  :custom
  (evil-echo-state . nil)
  (evil-lookup-func 'pada/evil-lookup-func)
  :config
  (unbind-key "C-k" 'evil-insert-state-map)
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  ;; (define-key evil-insert-state-map (kbd "<tab>") 'tab-to-tab-stop)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-nerd-commenter
  :after evil
  :config
  (general-define-key :states 'normal "gcc" 'evilnc-comment-or-uncomment-lines)
  (general-define-key :states 'visual "gc" 'evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-goggles
  :after evil
  :config
  (setq evil-goggles-duration 0.05)
  (evil-goggles-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package origami
  :config (global-origami-mode))

(provide 'init-evil)
;;; init-evil.el ends here
