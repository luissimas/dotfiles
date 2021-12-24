;; Setting garbage colector threshold
(setq gc-cons-threshold (* 100 1024 1024))

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(defun pada/display-startup-time ()
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

;; Quick yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

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
(setq visible-bell nil)

;; Fringes
(set-fringe-mode '(10 . 10))

;; Remember cursor position
(save-place-mode 1)

;; Spaces over tabs
(setq standard-indent 2)
(setq backward-delete-char-untabify-method 'hungry)
(setq indent-line-function 'insert-tab)
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

;; Font configuration
(defvar pada/default-font-size 120)
(defvar pada/default-font-family "Iosevka Padawan")

(defvar pada/variable-font-size 120)
(defvar pada/variable-font-family "Iosevka Padawan")

(set-face-attribute 'default nil :font pada/default-font-family :height pada/default-font-size :weight 'normal)
(set-face-attribute 'fixed-pitch nil :font pada/default-font-family :height pada/default-font-size :weight 'normal)
(set-face-attribute 'variable-pitch nil :font pada/variable-font-family :height pada/variable-font-size :weight 'light)

;; Custom function to kill current buffer
(defun pada/kill-buffer ()
  (interactive) (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'pada/kill-buffer)

;; Custom find-file
(defun pada/find-file ()
  "Wrapper around `find-file'.  If the current file is in a project, use `project-find-file', otherwise use the built-in `find-file'."
  (interactive)
  (if (project-current)
      (project-find-file)
    (call-interactively 'find-file)))

(global-set-key (kbd "C-x f") 'pada/find-file)

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
    :keymaps '(normal visual)
    :prefix "SPC")
  (pada/leader-key
    "x" '(execute-extended-command :which-key "M-x")
    "h" (general-simulate-key "C-h" :which-key "Help")
    "f" '(:ignore t :which-key "Find")
    "ff" '(pada/find-file :which-key "Find file")
    "fF" '(find-file :which-key "Find file in CWD")
    "fc" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Find config")
    "fs" '(save-buffer :which-key "Save file")
    "w" '(save-buffer :which-key "Save file")
    "q" '(evil-quit :which-key "Quit")
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "Switch buffer")
    "bk" '(pada/kill-buffer :which-key "Kill current buffer")
    "bK" '(kill-buffer :which-key "Kill buffer")
    "bi" '(ibuffer :which-key "Ibuffer"))
  ;; Window resizing
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
  evil-undo-system 'undo-tree
  evil-want-Y-yank-to-eol t
  evil-shift-width tab-width)
  (unbind-key "C-k" evil-insert-state-map)
  :hook
  (org-mode . (lambda () (define-key evil-normal-state-map (kbd "<tab>") 'evil-toggle-fold)))
  :config
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  (define-key evil-insert-state-map (kbd "<tab>") 'tab-to-tab-stop)
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
  :config
  (global-evil-surround-mode))

(use-package evil-nerd-commenter
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
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
  :config (which-key-mode)
  (setq which-key-idle-delay 1))

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
  (define-key magit-section-mode-map (kbd "<tab>") 'magit-section-toggle)
  (pada/leader-key
   "g" '(:ignore t :which-key "Git")
   "gs" 'magit-status))

;; Git gutter
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 1)
  (git-gutter:modified-sign "│")
  (git-gutter:added-sign "│")
  (git-gutter:deleted-sign "│"))

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
          (project-eshell "Eshell")))
  (pada/leader-key
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

;; Themes
(use-package modus-themes
  :straight nil
  :init
  (setq modus-themes-subtle-line-numbers t
        modus-themes-mode-line '(borderless))
  (load-theme 'modus-operandi))

(use-package nord-theme)

;; Icons
(use-package all-the-icons)

(use-package all-the-icons-dired
  :requires all-the-icons
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Better syntax highlight
(use-package tree-sitter
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay .5)
  :config
  (define-key evil-insert-state-map (kbd "C-SPC") 'completion-at-point)
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous))
  :hook (prog-mode . corfu-mode))

;; Setting font faces
(defun pada/set-fonts ()
  "Set the main font faces."
  (set-face-attribute 'default nil :font "Iosevka Padawan 12")
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Padawan 12")
  (set-face-attribute 'variable-pitch nil :font "Iosevka Padawan 12"))

;; Setting frame options in both daemon (with hooks) or on
;; normal emacs startup (directly calling the functions)
(if (daemonp)
    (progn (add-hook 'server-after-make-frame-hook 'pada/set-fonts))
  (progn (pada/set-fonts)))

;; Use frames instead of windows (better integration with tiling wm)
(setq pop-up-frames 'graphic-only)

;; NOTE: I really need to understand all of this better
(setq display-buffer-alist
      '(("\\`\\*Calendar\\*\\'"
         (display-buffer-below-selected))
        ("\\`\\*Async Shell Command\\*\\'"
         (display-buffer-no-window))
        ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp.*\\|Messages\\)\\*"
        (display-buffer-in-side-window)
        (window-height . 0.3)
        (side . bottom)
        (slot . 0))
        ("\\*\\(e?shell\\|vterm\\)\\*"
        (display-buffer-in-side-window)
        (window-height . 0.3)
        (side . bottom)
        (slot . -1))
        ("\\`magit-diff:.*\\'"
         (display-buffer-pop-up-window))))

(setq display-buffer-base-action nil)
      ;; '((display-buffer-reuse-window
      ;;    display-buffer-reuse-mode-window
      ;;    display-buffer-at-bottom) . ((mode . (helpful-mode help-mode)))))

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

;; Better terminal
(use-package vterm)
