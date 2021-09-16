;; Disabling init screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "padawan")
;; (setq initial-scratch-message nil)

;; Empty echo area startup message to clean it
(defun display-startup-echo-area-message ()
  (message ""))

;; Disabling main UI components
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(tooltip-mode t)
(set-fringe-mode 10)

;; Setting custom directory
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p (concat user-emacs-directory "custom.el"))
  (load custom-file))

;; Setting font
(set-face-attribute 'default nil :font "Iosevka-12")
(set-face-attribute 'fixed-pitch nil :font "Iosevka-12")
(set-face-attribute 'variable-pitch nil :font "Iosevka Aile-12")

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Setting up tabs
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 200 2))
(setq tab-width 2)

;; Window movement
;; (global-set-key (kbd "C-h") 'windmove-left)
;; (global-set-key (kbd "C-j") 'windmove-down)
;; (global-set-key (kbd "C-k") 'windmove-up)
;; (global-set-key (kbd "C-l") 'windmove-right)
;; (global-set-key (kbd "C-s") 'split-window-horizontally)

;; Line numbers
(column-number-mode)

;; Enable line numbers and truncate lines only on programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (setq display-line-numbers 'relative)
                            (toggle-truncate-lines)))

;; Enable autopairs on programming modes
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Enable soft wrap for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; Disable line numbers for some modes
;;(dolist (mode '(org-mode-hook
;;		term-mode-hook
;;		shell-mode-hook
;;		eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Configuring MELPA
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Setting up use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Setting up emacs path
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; General for keybindings
(use-package general
  :config
  (general-create-definer pada/nmap
    :keymaps '(normal emacs)
    :prefix "SPC"))

;; Evil-mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Ivy and Counsel
(use-package ivy
  :demand
  :diminish
  :bind (:map ivy-minibuffer-map
	      ("C-j" . ivy-next-line)
	      ("C-k" . ivy-previous-line))
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

;; FLX to sort ivy's fzf results
(use-package flx)

;; Smex to sort M-x by usage history
(use-package smex)

(use-package counsel
  :demand
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file))
  :config
  (pada/nmap
    "f" '(:ignore t :which-key "find")
    "ff" 'counsel-find-file
    "." 'counsel-find-file
    "fb" 'counsel-ibuffer
    "," 'counsel-ibuffer))

(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode))

;; Better help pages
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Themes
(use-package doom-themes
  :config (load-theme 'doom-palenight t))

;; Projectile
(use-package projectile
  :custom (projectile-completion-system 'ivy)
  (when (file-directory-p "~/fun")
    (setq projectile-project-search-path '("~/fun")))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (pada/nmap
    "g" '(:ignore t :which-key "git")
    "gs" 'magit-status))

;; Icons
(use-package all-the-icons)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq display-time-day-and-date t
        display-time-format "%a %d/%m %H:%M"
        display-time-default-load-average nil
        doom-modeline-buffer-encoding nil)
  (display-time-mode)
  (display-battery-mode))

;; Which-key
(use-package which-key
  :config (which-key-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Eglot LSP client
(use-package eglot
  :hook
  (js-mode . eglot-ensure)
  (tuareg-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  :config
  (setq eglot-ignored-server-capabilities '(:hoverProvider)))


;; Company completion
(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete))

;; Flycheck for syntax checking
;;(use-package flycheck
;;  :config
;;  (global-flycheck-mode))

;; Code formatter
(use-package format-all
  :hook (prog-mode . format-all-mode)
  :hook (format-all-mode . format-all-ensure-formatter))

;; Setup tabs and other things for projects
(use-package editorconfig
  :config
  (editorconfig-mode))

;; Treesitter for better syntax highlight
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs)

;; Git line status
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│"))

;; Prettier integration
(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

;; Making emacs search for binaries in node_modules
(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

;; Ocaml setup
(use-package tuareg
  :config
  (put 'tuareg-mode 'eglot--language-id "ocaml"))

(use-package utop
  :config
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (setq utop-command "opam config exec -- dune utop . -- -emacs"))

;; Display eldoc information in a floating window
;;(use-package eldoc-box
;;  :config
;;  (eldoc-box-hover-at-point-mode))

;; Displaying flymake errors on minibuffer
(custom-set-variables
 '(help-at-pt-display-when-idle t))

;; Better pdf view and general tools
(use-package pdf-tools
  :config (pdf-tools-install))

;; Disable border caused by cursor in pdf-view
(add-hook 'pdf-view-mode-hook
          (lambda ()
            (set (make-local-variable 'evil-normal-state-cursor) (list nil))))

;; Display inline latex formulas and images
(use-package texfrag
  :hook
  (texfrag-mode . texfrag-document)
  (markdown-mode . texfrag-mode)
  (latex-mode . texfrag-mode))

(use-package markdown-mode
  :init
  (setq-default markdown-hide-markup t
                markdown-enable-wiki-links t
                markdown-enable-math t
                markdown-wiki-link-alias-first nil
                markdown-wiki-link-search-subdirectories t
                markdown-link-space-sub-char " "))

(use-package olivetti
  :hook
  (markdown-mode . olivetti-mode)
  (latex-mode . olivetti-mode))
