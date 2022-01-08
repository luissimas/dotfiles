;; Setting garbage colector threshold
(setq gc-cons-threshold (* 100 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(defun pada/display-startup-time ()
  "Display the startup time for the current section."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook 'pada/display-startup-time)

;; Setting the correct native compilation path
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Supressing native compilation warnings
(setq native-comp-async-report-warnings-errors nil
      warning-minimum-level :emergency)

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

;; Stop asking for following git symlink
(setq vc-follow-symlinks t)

;; Prompts and confirmation
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Disable suggestion for keybindings in minibuffer
(setq suggest-key-bindings nil)

;; Disabling init screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Disabling main UI components
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(setq visible-bell nil
      use-dialog-box nil)
(setq-default cursor-in-non-selected-windows nil)

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Fringes
(set-fringe-mode '(5 . 5))

;; Cursor offset
(setq scroll-margin 8
      scroll-conservatively 101)

;; Remember cursor position
(save-place-mode 1)

;; Spaces over tabs
(setq standard-indent 2)
(setq backward-delete-char-untabify-method 'hungry)
(setq indent-line-function 'insert-tab)
(setq tab-always-indent nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default electric-indent-inhibit t)

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Unique buffer name formats
(setq uniquify-buffer-name-style 'forward)

;; Column numbers
(column-number-mode)

;; Remove truncation and continuation indicators
(setq-default fringe-indicator-alist
              (assq-delete-all 'continuation
                               (assq-delete-all 'truncation fringe-indicator-alist)))

;; Enable line numbers and truncate lines only on programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (setq display-line-numbers 'relative)
                            (toggle-truncate-lines)))

;; Enable autopairs on programming modes
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)

;; Enable soft wrap for text modes
(add-hook 'text-mode-hook 'visual-line-mode)

;; Desktop/Laptop distinction
(defun pada/is-laptop ()
  "Returns `t' if the current session is running on a laptop with battery, otherwise returns `nil'."
  (file-exists-p "/sys/class/power_supply/BAT1"))

;; Font configuration
(defvar pada/default-font-size (if (pada/is-laptop) 100 120))
(defvar pada/default-font-family "Iosevka Padawan")

(defvar pada/variable-font-size (if (pada/is-laptop) 100 120))
(defvar pada/variable-font-family "Iosevka Padawan")

;; Custom function to kill current buffer
(defun pada/kill-current-buffer ()
  "Kill the current buffer."
  (interactive) (kill-buffer (current-buffer)))

;; Custom find-file
(defun pada/find-file ()
  "Wrapper around `find-file'.  If the current file is in a project, use `project-find-file', otherwise use the built-in `find-file'."
  (interactive)
  (if (project-current)
      (project-find-file)
    (call-interactively 'find-file)))

;; Theme utils
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

(defcustom pada/system-theme-associations
  '(("modus-operandi" modus-operandi)
    ("modus-vivendi" modus-vivendi)
    ("nord" doom-nord)
    ("gruvbox" doom-gruvbox)
    ("palenight" doom-palenight))
  "A alist of association between file patterns and external programs."
  :group 'system-theme
  :type "alist")

(defun pada/load-system-theme ()
  "Read file ~/.colorscheme and load its theme."
  (interactive)
  (with-temp-buffer
    (insert-file-contents "~/.colorscheme")
    (let ((theme (string-trim (buffer-string)))
          (associations pada/system-theme-associations))
      (while associations
        (let* ((current (pop associations))
               (system-theme (car current))
               (emacs-theme (car (cdr current))))
          (when (string-match-p system-theme theme)
            (pada/load-theme emacs-theme)
            (setq associations nil)))))))

;; Straight setup
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

;; Keybindings
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  (general-create-definer pada/leader-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC")
  ;; Main keybingins, got a lot of inspiration from Doom Emacs (default/+evil-bindings.el)
  (pada/leader-key
    "h" '(:keymap help-map :which-key "Help")
    "w" '(:keymap evil-window-map :which-key "Window")

    "x" '(execute-extended-command :which-key "M-x")
    "u" '(universal-argument :which-key "Universal argument")

    "f" '(:ignore t :which-key "Find")
    "ff" '(pada/find-file :which-key "Find file")
    "fg" '(consult-ripgrep :which-key "Grep")
    "fF" '(find-file :which-key "Find file in CWD")
    "fc" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Find config")
    "fC" '(editorconfig-find-current-editorconfig :which-key "Find project editorconfig")
    "fs" '(save-buffer :which-key "Save file")
    "fS" '(write-file :which-key "Save file as...")

    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(pada/kill-current-buffer :which-key "Kill current buffer")
    "bK" '(kill-buffer :which-key "Kill buffer")
    "bi" '(ibuffer :which-key "Ibuffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")
    "bN" '(evil-buffer-new :which-key "New buffer")
    "br" '(revert-buffer :which-key "Revert buffer")

    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Magit status")
    "gc" '(magit-clone :which-key "Magit clone")
    "gl" '(magit-log-buffer-file :which-key "Magit buffer log")
    "gi" '(magit-init :which-key "Magit init")

    "p" '(:keymap project-prefix-map :which-key "Project")
    ;; "p!" 'project-shell-command
    "pa" 'project-async-shell-command
    "p&" nil

    "t" '(:ignote t :which-key "Toggle")
    "tt" '(pada/load-theme :which-key "Theme")
    "tf" '(flycheck-mode :which-key "Flycheck")
    "tg" '(evil-goggles-mode :which-key "Evil goggles"))

  ;; Window resizing
  ;; TODO: Replace it with a hydra
  (general-define-key
   "M-h" 'shrink-window-horizontally
   "M-j" 'shrink-window
   "M-k" 'enlarge-window
   "M-l" 'enlarge-window-horizontally))

;; Evil-mode
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-minibuffer t
        evil-undo-system 'undo-tree
        evil-want-Y-yank-to-eol t
        evil-shift-width tab-width)
  (unbind-key "C-k" evil-insert-state-map)
  :hook
  (org-mode . (lambda () (define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)))
  :config
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  ;; (define-key evil-insert-state-map (kbd "<tab>") 'tab-to-tab-stop)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (setq evil-lookup-func (lambda ()
                           (cond
                            ((and (boundp 'lsp-ui-doc-frame-mode) lsp-ui-doc-frame-mode) (lsp-ui-doc-focus-frame))
                            ((and (boundp 'lsp-mode) lsp-mode) (lsp-ui-doc-glance))
                            ((equal major-mode #'emacs-lisp-mode) (helpful-at-point))
                            (t woman))))
  (evil-mode 1))

(use-package evil-surround
  :requires evil
  :hook
  (evil-mode . evil-surround-mode))

(use-package evil-nerd-commenter
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-goggles
  :config
  (setq evil-goggles-duration 0.05)
  (evil-goggles-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Undo tree
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  (global-undo-tree-mode))

;; Which key
(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (setq which-key-idle-delay 0.5)
  (setq which-key-add-column-padding 5)
  (which-key-mode))

;; Helpful
(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (define-key magit-section-mode-map (kbd "<tab>") 'magit-section-toggle))

;; Git gutter
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│")
  (set-face-attribute 'git-gutter:unchanged nil :background nil :inherit 'default))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [240] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:deleted [240] nil nil '(center t))
  (define-fringe-bitmap 'git-gutter-fr:modified [240] nil nil '(center t))
  (set-face-attribute 'git-gutter-fr:added nil :inherit 'magit-diff-added-highlight)
  (set-face-attribute 'git-gutter-fr:modified nil :inherit 'magit-diff-base-highlight)
  (set-face-attribute 'git-gutter-fr:deleted nil :inherit 'magit-diff-removed-highlight))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; Show colors
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

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

  (evil-define-key '(normal insert) 'minibuffer-mode-map (kbd "C-j") 'vertico-next)
  (evil-define-key '(normal insert) 'minibuffer-mode-map (kbd "C-k") 'vertico-previous)
  (evil-define-key 'normal 'minibuffer-mode-map (kbd "<escape>") 'abort-minibuffers)

  ;; Using vertico-directory extension
  (add-to-list 'load-path (expand-file-name "straight/build/vertico/extensions" user-emacs-directory))
  (require 'vertico-directory)
  :init
  (vertico-mode))

;; Completion style
(use-package orderless
  :config
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-flex orderless-regexp)))

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

;; Better completion commands
(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line))
  :config
  (setq consult-narrow-key (kbd "C-."))
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; Project management
(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-switch-to-buffer "Switch to buffer")
          (project-dired "Dired")
          (project-eshell "Eshell"))))

;; Themes
(use-package modus-themes
  :straight nil
  :init
  (setq modus-themes-subtle-line-numbers t
        modus-themes-mode-line '(borderless)))

(use-package nano-theme)

(use-package doom-themes
  :custom
  (doom-gruvbox-dark-variant "hard"))

;; Icons
(use-package all-the-icons)

(use-package all-the-icons-dired
  :requires all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Better syntax highlight
(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

;; At-point completion
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay .2)
  :config
  (define-key evil-insert-state-map (kbd "C-SPC") 'completion-at-point)
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :hook (prog-mode . corfu-mode))

;; (use-package corfu-doc
;;   :straight '(:host github :repo "galeo/corfu-doc"))

;; (use-package kind-icon
;;   :requires svg-lib
;;   :after corfu
;;   :custom
;;   (kind-icon-deafult-face 'corfu-default)
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Setting font faces
(defun pada/set-fonts ()
  "Set the main font faces."
  (interactive)
  (set-face-attribute 'default nil :font pada/default-font-family :height pada/default-font-size :weight 'normal)
  (set-face-attribute 'fixed-pitch nil :font pada/default-font-family :height pada/default-font-size :weight 'normal)
  (set-face-attribute 'variable-pitch nil :font pada/variable-font-family :height pada/variable-font-size :weight 'light))

;; Frame parameters
(defvar pada/frame-parameters
  '((no-special-glyphs t)
    (internal-border-width 0)))

(setq frame-resize-pixelwise t)

(defun pada/set-frame-parameters ()
  "Set parameters defined in `pada/frame-parameters' for the current frame."
  (interactive)
  (dolist (parameter pada/frame-parameters)
    (set-frame-parameter (selected-frame) (car parameter) (car (cdr parameter)))))

;; Setting frame options in both daemon (with hooks) or on
;; normal emacs startup (directly calling the functions)
(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook 'pada/set-fonts)
      (add-hook 'server-after-make-frame-hook 'pada/set-frame-parameters))
  (progn
    (pada/set-fonts)
    (pada/set-frame-parameters)))

;; NOTE: I really need to understand all of this better
(setq display-buffer-alist
      '(("\\`\\*Calendar\\*\\'"
         (display-buffer-below-selected))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|compilation\\|Messages\\|Async Shell Command\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . 0))
        ("\\*\\([Hh]elp.*\\|info\\)\\*"
         (display-buffer-in-side-window)
         (window-width . 0.4)
         (side . right)
         (slot . 0))
        ("\\*\\(.*e?shell\\|vterm\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.3)
         (side . bottom)
         (slot . -1))))

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

;; Opening files in external commands based on the filename
(defgroup pada/open-external nil
  "Open files with external commands."
  :group 'files
  :group 'processes)

(defcustom pada/open-external-associations
  '(("\\.pdf\\'\\|\\.epub\\'\\|\\.djvu\\'" "zathura")
    ("\\.mkv\\'\\|\\.mp4\\'" "mpv"))
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
      (let* ((current (pop associations))
             (pattern (car current))
             (program (car (cdr current))))
        (when (string-match-p pattern file-name)
          (pada/run-shell-command (concat program " " (shell-quote-argument file-name)))
          (setq found t)
          (setq associations nil))))
    (unless found
      (apply fun args))))

(advice-add 'find-file :around 'pada/open-external-advice)

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

;; MPC, most stuff grabed from here: https://pspiagicw.github.io/posts/the-weirdest-mode-in-emacs-mpc-mode/
(defun pada/mpc-move-down ()
  (interactive)
  (evil-next-visual-line)
  (mpc-select))

(defun pada/mpc-move-up ()
  (interactive)
  (evil-previous-visual-line)
  (mpc-select))

(general-define-key
 :keymaps 'mpc-mode-map
 :states 'normal
 "j" 'pada/mpc-move-down
 "k" 'pada/mpc-move-up
 "p" 'mpc-toggle-play
 "r" 'mpc-toggle-repeat
 "s" 'mpc-toggle-shuffle
 "c" 'mpc-toggle-consume
 "a" 'mpc-playlist-add
 ">" 'mpc-next
 "<" 'mpc-prev
 "R" 'mpc-playlist-delete
 "RET" 'mpc-select
 "x" 'mpc-play-at-point)

;; Mode line
(use-package doom-modeline
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 4)
  (doom-modeline-minor-modes nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  :init
  (doom-modeline-mode))

;; Time display format
(setq display-time-format "%A %d %b, %H:%M")
(setq display-time-default-load-average nil)
;; (display-time-mode)

;; Org mode
(use-package org-bullets
  :hook (org-mode . (lambda () (org-bullets-mode 1))))

;; LSP-mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c c")
  :hook
  (js-mode . lsp)
  :commands lsp
  :custom
  (lsp-lens-place-position 'above-line)
  (lsp-elixir-dialyzer-enabled nil)
  :config
  (setq read-process-output-max (* 1024 1024)
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

;; Flycheck for syntax checking
(use-package flycheck
  :hook (js-mode . flycheck-mode))

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

;; Making emacs search for binaries in node_modules
(use-package add-node-modules-path
  :hook (js-mode . add-node-modules-path))

;; Highlight todo comments
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       font-lock-constant-face bold)
          ("WARNING"    warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold))))

;; Better terminal emulator
(use-package vterm
  :config
  (setq vterm-shell "/bin/bash"))

;; Load system theme on startup
(pada/load-system-theme)
