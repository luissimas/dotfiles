;; Disabling init screen
(setq inhibit-startup-message t)

;; Disabling main UI components 
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(tooltip-mode t)
(set-fringe-mode 0)

;; Setting custom directory
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Setting font
(set-face-attribute 'default nil :font "JetBrains Mono" :height 110)

;; Setting theme
(load-theme 'doom-palenight)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Ivy
(use-package ivy
  :diminish
  :bind (:map ivy-minibuffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line))
  :demand
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1))

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
