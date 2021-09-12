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
(tooltip-mode t)
(set-fringe-mode 10)

;; Setting custom directory
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p (concat user-emacs-directory "custom.el"))
      (load custom-file))

;; Setting font
(set-face-attribute 'default nil :font "JetBrains Mono" :height 110)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Window movement
;; (global-set-key (kbd "C-h") 'windmove-left)
;; (global-set-key (kbd "C-j") 'windmove-down)
;; (global-set-key (kbd "C-k") 'windmove-up)
;; (global-set-key (kbd "C-l") 'windmove-right)
;; (global-set-key (kbd "C-s") 'split-window-horizontally)

;; Line numbers
(column-number-mode)

;; Enable line numbers only on programming modes
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))

;; Enable autopairs on programming modes
(add-hook 'prog-mode-hook 'electric-pair-mode)

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
  :custom
  (ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

;; FLX to sort ivy's fzf results
(use-package flx)

(use-package counsel
  :demand
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)
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
  :init (doom-modeline-mode 1))

;; Which-key
(use-package which-key
  :config (which-key-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Eglot LSP client
(use-package eglot
  :config
  (add-hook 'js-mode-hook 'eglot-ensure))

;; Company completion
(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete))

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
