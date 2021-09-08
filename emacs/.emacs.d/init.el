;; Disabling init screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "padawan")
(setq initial-scratch-message nil)

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

;; Setting theme
(load-theme 'doom-palenight)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line numbers
(column-number-mode)

;; Enable line numbers only on programming modes
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))

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

;; Ivy and Counsel
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line))
  :demand
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode))


;; Themes
(use-package doom-themes)

;; Evil-mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Magit
(use-package magit)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Which-key
(use-package which-key
  :config (which-key-mode))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
