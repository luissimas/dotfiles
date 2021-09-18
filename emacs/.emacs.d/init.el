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

;; Setting font faces
(defun pada/set-fonts ()
  (set-face-attribute 'default nil :font "JetBrains Mono-11")
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono-11")
  (set-face-attribute 'variable-pitch nil :font "Open Sans-12"))

;; Setting font ligatures
(let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   (?*  . ,(regexp-opt '("*>" "***" "*/")))
                   (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                         "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                         "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                   (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  . ,(regexp-opt '("&&&" "&&")))
                   (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  . ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ . ,(regexp-opt '("{|")))
                   (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; . ,(regexp-opt '(";;")))
                   (?_  . ,(regexp-opt '("_|_" "__")))
                   (?\\ . ,(regexp-opt '("\\" "\\/")))
                   (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  . ,(regexp-opt '("$>")))
                   (?^  . ,(regexp-opt '("^=")))
                   (?\] . ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(add-hook 'server-after-make-frame-hook 'pada/set-fonts)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Keymap to edit init.el
(global-set-key (kbd "C-c d") (lambda () (interactive) (find-file "~/dotfiles/emacs/.emacs.d/init.el")))

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

;; Configuring straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

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
  (setq evil-want-Y-yank-to-eol t)
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
;; (use-package eglot
;;   :hook
;;   (js-mode . eglot-ensure)
;;   (tuareg-mode . eglot-ensure)
;;   (c-mode . eglot-ensure)
;;   (c++-mode . eglot-ensure)
;;   :config
;;   (setq eglot-ignored-server-capabilities '(:hoverProvider)))

;; LSP-mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c c")
  :hook
  (js-mode . lsp)
  (tuareg-mode . lsp)
  :commands lsp
  :config
  (setq read-process-output-max (* 1024 1024)
        gc-cons-threshold 100000000
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-enable-symbol-highlighting t
        lsp-eldoc-render-all nil)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-signature-render-documentation nil)
  (define-key evil-normal-state-map (kbd "K") 'lsp-ui-doc-glance))

;; Company completion
(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete))

;; Flycheck for syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

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

;; Elixir setup
(use-package elixir-mode)

;; Display eldoc information in a floating window
;;(use-package eldoc-box
;;  :config
;;  (eldoc-box-hover-at-point-mode))

;; Displaying flymake errors on minibuffer
(custom-set-variables
 '(help-at-pt-display-when-idle t))

;; Better pdf view and general tools
(use-package pdf-tools
  :config
  (pdf-tools-install)
  ;; Disable border caused by cursor in pdf-view
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))

;; Remember pdf page
(use-package pdf-view-restore
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))


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
                markdown-wiki-link-search-parent-directories t
                markdown-link-space-sub-char " "))

(use-package olivetti
  :hook
  (markdown-mode . olivetti-mode)
  (latex-mode . olivetti-mode)
  :config
  (add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 0.6))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package counsel-spotify
  :config
  (setq counsel-spotify-client-id pada/spotify-client-id
        counsel-spotify-client-secret pada/spotify-client-secret
        counsel-spotify-service-name "mopidy"
        counsel-spotify-use-notifications nil
        counsel-spotify-use-system-bus-p nil)
  (pada/nmap
    "s" '(:ignore t :which-key "spotify")
    "sp" 'counsel-spotify-toggle-play-pause
    "st" 'counsel-spotify-search-track
    "sa" 'counsel-spotify-search-album))

;; Screenshots
(use-package screenshot
  :straight '(screenshot :host github :repo "tecosaur/screenshot")
  :config
  (global-set-key (kbd "C-c s") #'screenshot))
