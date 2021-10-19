;;; Init.el --- My Emacs config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Setting the correct native compilation path
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Disabling init screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "padawan")
;; (setq initial-scratch-message nil)

;; Disabling x-resources
(setq inhibit-x-resources t)

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

;; Setting backup and auto-save directories
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves" user-emacs-directory) t)))

(setq create-lockfiles nil)

;; Setting unique buffer names
(setq uniquify-buffer-name-style 'forward)

;; Setting frame options
(defun pada/set-window-divider ()
  "Set window-divider options."
  (setq window-divider-default-right-width 2)
  (setq window-divider-default-bottom-width 0)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1))

;; Setting font faces
(defun pada/set-fonts ()
  "Set the main font faces."
  (set-face-attribute 'default nil :font "Iosevka Padawan-12")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Padawan-12")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile-12"))

;; Setting frame options in both daemon (with hooks) or
;; normal emacs startup (directly calling the functions)
(if (daemonp)
    (progn (add-hook 'server-after-make-frame-hook 'pada/set-fonts)
           (add-hook 'server-after-make-frame-hook 'pada/set-window-divider))
  (progn (pada/set-fonts)
         (pada/set-window-divider)))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Keymap to edit init.el
(global-set-key (kbd "C-c d") (lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))))

;; Custom function to kill current buffer
(defun pada/kill-buffer ()
  (interactive) (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'pada/kill-buffer)

;; Setting up tabs
(setq-default indent-tabs-mode nil)
(setq tab-stop-list (number-sequence 2 200 2))
(setq tab-width 2)

;; Line numbers
(column-number-mode)

;; Enable line numbers and truncate lines only on programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (setq display-line-numbers 'relative)
                            (toggle-truncate-lines)))

;; Enable autopairs on programming modes
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Enable soft wrap for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

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

(defun pada/find-file ()
  "Wrapper around `find-file'.  If the current file is in a project, use `project-find-file', otherwise use the built-in `find-file'."
  (interactive)
  (if (project-current)
      (project-find-file)
    (call-interactively 'find-file)))

;; General for keybindings
(use-package general
  :config
  (general-create-definer pada/nmap
    :keymaps 'normal
    :prefix "SPC")
  (pada/nmap
    "x" '(execute-extended-command :which-key "M-x")
    "h" (general-simulate-key "C-h" :which-key "Help")
    "w" (general-simulate-key "C-w" :which-key "Window")
    "f" '(:ignore t :which-key "Find")
    "ff" '(pada/find-file :which-key "Find file")
    "fF" '(find-file :which-key "Find file in CWD")
    "fc" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Find config")
    "fs" '(save-buffer :which-key "Save file")
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(pada/kill-buffer :which-key "Kill current buffer")
    "bK" '(kill-buffer :which-key "Kill buffer")
    "bi" '(ibuffer :which-key "Ibuffer")))

(global-set-key (kbd "C-x k") 'pada/kill-buffer)

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
  (define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (setq evil-lookup-func (lambda ()
                           (cond
                            ((and (boundp 'lsp-ui-doc-frame-mode) lsp-ui-doc-frame-mode) (lsp-ui-doc-focus-frame))
                            ((and (boundp 'lsp-mode) lsp-mode) (lsp-ui-doc-glance))
                            ((equal major-mode #'emacs-lisp-mode) (helpful-at-point))
                            (t woman))))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Vertico as the completion UI
(use-package vertico
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char))
  :config
  (setq vertico-cycle t
        completion-styles '(basic substring flex)
        completion-ignore-case t)

  ;; Using vertico-directory extension
  (add-to-list 'load-path (expand-file-name "straight/build/vertico/extensions" user-emacs-directory))
  (require 'vertico-directory)
  :init
  (vertico-mode))

;; Completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Richer completion annotations
(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Actions on completion items
(use-package embark
  :bind (("M-o" . embark-act)
         ("C-h b" . embark-bindings))
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
                            The which-key help message will show the type and value of the
                            current target followed by an ellipsis if there are further
                            targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (kill-buffer which-key--buffer)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Better completion commands
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line))
  :config
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; Better help pages
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package bespoke-themes
  :straight (:host github :repo "mclear-tools/bespoke-themes" :branch "main")
  :config
  (setq bespoke-set-mode-line nil
        bespoke-set-mode-line-cleaner t
        bespoke-set-evil-cursors t
        bespoke-set-italic-comments t
        bespoke-set-italic-keywords nil))

;; Dim non-active windows
;;(use-package dimmer
;;  :straight (:host github :repo "gonewest818/dimmer.el")
;;  :config
;;  (setq dimmer-fraction 0.3)
;;  (setq dimmer-adjustment-mode :foreground)
;;  (setq dimmer-use-colorspace :rgb)
;;  (setq dimmer-watch-frame-focus-events t)
;;  (dimmer-configure-which-key)
;;  (dimmer-configure-magit)
;;  (dimmer-configure-org)
;;  (dimmer-configure-posframe)
;;  :init (dimmer-mode 1))

;; Projectile
;; (use-package projectile
;;   :custom (projectile-completion-system 'default)
;;   (when (file-directory-p "~/fun")
;;     (setq projectile-project-search-path '("~/fun")))
;;   :config
;;   (pada/nmap
;;     "p" '(:keymap projectile-command-map :package projectile :which-key "Projectile"))
;;   (projectile-mode))
(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-switch-to-buffer "Switch to buffer")
          (project-dired "Dired")
          (project-eshell "Eshell")))
  (pada/nmap
    "p" '(:ignore t :which-key "Project")
    "p!" 'project-shell-command
    "pa" 'project-async-shell-command
    "pf" 'project-find-file
    "pF" 'project-or-external-find-file
    "pb" 'project-switch-to-buffer
    "ps" 'project-shell
    "pd" 'project-dired
    "pv" 'project-vc-dir
    "pc" 'project-compile
    "pe" 'project-eshell
    "pk" 'project-kill-buffers
    "pp" 'project-switch-project
    "pg" 'project-find-regexp
    "pG" 'project-or-external-find-regexp
    "pr" 'project-query-replace-regexp
    "px" 'project-execute-extended-command))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (define-key magit-section-mode-map (kbd "<tab>") 'magit-section-toggle)
  (pada/nmap
    "g" '(:ignore t :which-key "Git")
    "gs" 'magit-status))

;; Icons
(use-package all-the-icons)

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil))
;;  :config
;;  (setq display-time-day-and-date t
;;        display-time-format "%a %d/%m %H:%M"
;;        display-time-default-load-average nil)
;;  (display-time-mode)
;;  (display-battery-mode))

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
  (elixir-mode . lsp)
  (python-mode . lsp)
  (c-mode . lsp)
  :commands lsp
  :custom
  (lsp-lens-place-position 'above-line)
  (lsp-elixir-dialyzer-enabled nil)
  :config
  (setq read-process-output-max (* 1024 1024)
        gc-cons-threshold 100000000
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        lsp-enable-symbol-highlighting t
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-eldoc-render-all nil)
  (set-face-attribute 'lsp-details-face nil :height 0.9)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-max-width 400
        lsp-ui-doc-max-height 30
        lsp-signature-render-documentation nil)
  (define-key evil-normal-state-map (kbd "C-k") 'lsp-ui-doc-focus-frame)
  (evil-define-key 'normal 'lsp-ui-doc-frame-mode
    [?q] #'lsp-ui-doc-unfocus-frame))

;; Company completion
(use-package company
  :hook
  (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay nil)
  :config
  (evil-define-key 'insert 'company-mode (kbd "C-SPC") 'company-complete))

;; Flycheck for syntax checking
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;; Code formatter
(use-package format-all
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter)
  :config
  (setq format-all-show-errors 'never))

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
;; (use-package prettier-js
;;   :hook (js-mode . prettier-js-mode)
;;   :config
;;   (setq prettier-js-show-errors nil))

;; Making emacs search for binaries in node_modules
(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

;; Ocaml setup
(use-package tuareg
  :hook (tuareg-mode . (lambda () (add-to-list 'prettify-symbols-alist '("fun" . 955))))
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
;; (use-package pdf-tools
;;   :config
;;   (pdf-tools-install)
;;   ;; Disable border caused by cursor in pdf-view
;;   (add-hook 'pdf-view-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'evil-normal-state-cursor) (list nil)))))

;; Remember pdf page
;; (use-package pdf-view-restore
;;   :after pdf-tools
;;   :hook (pdf-view-mode . pdf-view-restore-mode)
;;   :config
;;   (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))


;; Display inline latex formulas and images
(use-package texfrag
  :hook
  (texfrag-mode . texfrag-document)
  (markdown-mode . texfrag-mode)
  (latex-mode . texfrag-mode))

(use-package markdown-mode
  :hook
  (markdown-mode . flyspell-mode)
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
  (org-mode . olivetti-mode)
  :config
  (add-hook 'olivetti-mode-on-hook (lambda () (olivetti-set-width 0.6))))

(require 'json)

(defun pada/get-private-key (key)
  "Return the value of the `key` in secret.json"
  (cdr (assoc key (json-read-file (expand-file-name "secret.json" user-emacs-directory)))))

;; Screenshots
(use-package screenshot
  :straight '(:host github :repo "tecosaur/screenshot")
  :bind (("C-c s" . #'screenshot)))

;; Better teminal
(use-package vterm)

;; Reading EPUBs
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; Habitica integration
;; (use-package habitica
;;   :config
;;   (setq habitica-uid (pada/get-private-key 'habitica-user-id)
;;         habitica-token (pada/get-private-key 'habitica-api-key)))

;; Change colorscheme variation based on the time of the day
(defun pada/auto-theme ()
  "Change the current bespoke theme variant based on the time of the day."
  (let ((time (string-to-number (substring (current-time-string) 11 13))))
    (cond ((and (>= time 18) (string-equal bespoke-set-theme "light"))
           (bespoke/dark-theme))
          ((and (< time 18) (string-equal bespoke-set-theme "dark"))
           (bespoke/light-theme)))))

;; Run auto-theme funcion every 5 minutes
;; (run-with-timer 0 300 #'pada/auto-theme)

;; Setting default dictionary
(setq ispell-dictionary "brasileiro")

;; Use frames instead of windows (trying this workflow with tiling wm)
(setq pop-up-frames 'graphic-only)

(defun pada/display-helpful-buffer (buffer alist)
  (if (s-matches-p "\\`\\*helpful.*\\'" (buffer-name))
      (display-buffer-same-window buffer alist)

    (display-buffer--maybe-pop-up-frame-or-window buffer alist)))

(setq display-buffer-alist
      '(("\\`\\*Calendar\\*\\'"
         (display-buffer-below-selected))
        ("\\`magit-diff:.*\\'"
         (display-buffer-pop-up-window))
        ("\\`\\*helpful.*\\'" (pada/display-helpful-buffer))))

(setq frame-auto-hide-function 'delete-frame)

;; Always kill the buffer when quitting a window
(global-set-key [remap quit-window] '(lambda () (interactive) (quit-window t)))
(global-set-key [remap magit-mode-bury-buffer] '(lambda () (interactive) (magit-mode-bury-buffer t)))

;; Kill magit diff buffer after commit
(defun pada/kill-magit-diff-buffer ()
  "Kill the magit-diff-buffer for the current repository, This function is meant to be added on `git-commit-setup-hook'."
  (defun kill-magit-diff-buffer ()
    (kill-buffer (magit-get-mode-buffer 'magit-diff-mode)))
  (add-hook 'with-editor-post-finish-hook 'kill-magit-diff-buffer nil t))

(add-hook 'git-commit-setup-hook 'pada/kill-magit-diff-buffer)

;; Org-mode
(use-package org
  :hook
  (org-mode . flyspell-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; References in org-mode
;; (use-package org-ref)

;; Practice typing
(use-package speed-type)

;; Spotify client
(use-package consult-spotify
  :config
  (setq espotify-client-id (pada/get-private-key 'spotify-client-id)
        espotify-client-secret (pada/get-private-key 'spotify-client-secret)
        espotify-service-name "mopidy"
        espotify-use-system-bus-p nil))

(defun pada/bspwm-colors (&rest _)
  "Apply pywal colors to bspwm."
  (start-process-shell-command "Bspwm colors" nil "~/.config/bspwm/scripts/bspwmcolors.sh >/dev/null"))

(use-package theme-magic
  :config
  (setq theme-magic--theming-functions '(load-theme))
  (advice-add 'theme-magic-from-emacs--wrapper :after 'pada/bspwm-colors)
  (theme-magic-export-theme-mode))

(defun pada/load-theme (theme)
  "Improvement over the default `load-theme'.  Load THEME and disable all themes that were loaded before."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))))
  (load-theme theme t)
  (dolist (theme (cdr custom-enabled-themes))
    (disable-theme theme)))

;; Font ligatures
(use-package ligature
  :straight '(:host github :repo "mickeynp/ligature.el")
  :config
  ;; Iosevka ligatures
  (ligature-set-ligatures 'prog-mode
                          '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "-->" "--->" "->-" ">-" ">>-"
                            "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "==>" "===>" "=>=" ">=" ">>="
                            "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "<!--" "<!---"
                            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "<>" "===" "!=="
                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+:" "-:" "=:" ":>" "__"
                            "(* *)" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!---" "<***>"))
  (global-ligature-mode))

;; Opening files in external commands based on the filename
(defgroup pada/open-external nil
  "Open files with external commands."
  :group 'files
  :group 'processes)

(defcustom pada/open-external-associations
  '(("\\.pdf\\'\\|\\.epub\\'\\|\\.djvu\\'" "zathura"))
  "A alist of association between file patterns and external programs."
  :group 'open-external
  :type "alist")

(defun pada/run-shell-command (command)
  "Run COMMAND in the default user shell."
  (message command)
  (start-process-shell-command "Open external process" nil (concat "exec nohup " command " >/dev/null")))

(defun pada/open-external-advice (fun &rest args)
  "Advice FUN with ARGS.
Try to match filename in ARGS against patterns in `open-external-associations',
if a pattern matches, then open the file using the specified command.  If no
pattern matches, simply call FUN with ARGS.
Note: This function is meant to be adviced around `find-file'."
  (let ((file-name (car args))
        (associations pada/open-external-associations)
        (found nil))
    (while associations
      (let* ((current (car associations))
             (pattern (car current))
             (program (car (cdr current))))
        (when (string-match-p pattern file-name)
          (pada/run-shell-command (concat program " " (shell-quote-argument file-name)))
          (setq found t)
          (setq associations nil)))
      (setq associations (cdr associations)))
    (unless found
      (apply fun args))))

(advice-add 'find-file :around 'pada/open-external-advice)

;;; Init.el ends here
