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
  :config
  (evil-mode 1))

;; Magit
(use-package magit)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" default))
 '(package-selected-packages
   '(magit doom-modeline evil evil-mode doom-themes use-package ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
