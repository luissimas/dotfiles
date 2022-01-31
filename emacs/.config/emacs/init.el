;; -*- lexical-binding: t; -*-
;; Setting garbage colector threshold
(setq gc-cons-threshold (* 100 1024 1024))

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
;; (setq confirm-kill-emacs 'y-or-n-p)

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
(set-fringe-mode '(5 . 0))

;; Cursor offset
(setq scroll-margin 8
      scroll-conservatively 101)

;; Remember cursor position
(save-place-mode 1)

;; One space is enough to end a sentence
(setq sentence-end-double-space nil)

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
                            (toggle-truncate-lines 1)))

;; Autopairs
(electric-pair-mode)

;; Desktop/Laptop distinction
(defun pada/is-laptop ()
  "Returns `t' if the current session is running on a laptop with battery, otherwise returns `nil'."
  (file-exists-p "/sys/class/power_supply/BAT1"))

;; Font configuration
(defvar pada/default-font-size (if (pada/is-laptop) 100 100))
(defvar pada/default-font-family "Iosevka Padawan")

(defvar pada/variable-font-size (if (pada/is-laptop) 1.2 1.2))
(defvar pada/variable-font-family "Fira Sans")

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

;; Load system theme on startup
(add-hook 'emacs-startup-hook 'pada/load-system-theme)

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

;; Setting up emacs path
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
    "wt" '(window-toggle-side-windows :which-key "Toggle side windows")

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
    "gb" '(magit-show-commit :which-key "Magit blame line")
    "gB" '(magit-blame :which-key "Magit blame")

    "p" '(:keymap project-prefix-map :which-key "Project")
    ;; "p!" 'project-shell-command
    "pa" 'project-async-shell-command
    "p&" nil

    "t" '(:ignote t :which-key "Toggle")
    "tt" '(pada/load-theme :which-key "Theme")
    "tf" '(flycheck-mode :which-key "Flycheck")
    "tg" '(git-gutter-mode :which-key "Git gutter")
    "tm" '(doom-modeline-mode :which-key "Doom modeline")
    "tr" '(rainbow-mode :which-key "Rainbow"))

  ;; Window resizing
  ;; TODO: Replace it with a hydra
  (general-define-key
   "M-h" 'shrink-window-horizontally
   "M-j" 'shrink-window
   "M-k" 'enlarge-window
   "M-l" 'enlarge-window-horizontally))

;; Evil-mode
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
  (unbind-key "C-k" evil-insert-state-map)
  :custom
  (evil-echo-state . nil)
  (evil-lookup-func 'pada/evil-lookup-func)
  :config
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
  :custom
  (fringes-outside-margins t)
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
(use-package rainbow-mode)

;; Vertico as the completion UI
(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  ;; Using vertico-directory extension
  (add-to-list 'load-path (expand-file-name "straight/build/vertico/extensions" user-emacs-directory))
  (require 'vertico-directory)
  (general-define-key
   :states '(normal insert)
   :keymaps 'vertico-map
   "C-j" 'vertico-next
   "C-k" 'vertico-previous
   "RET" 'vertico-directory-enter
   "DEL" 'vertico-directory-delete-char)
  (general-define-key
   :states 'normal
   :keymaps 'vertico-map
   "<escape>" 'abort-minibuffers)
  :init
  (vertico-mode))

;; Completion style
(defun pada/orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher for strings using the equal sign (`=') as a suffix."
  (when (string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1))))

(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-ignore-case t
        read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        orderless-matching-styles '(orderless-flex orderless-regexp)
        orderless-style-dispatchers '(pada/orderless-literal-dispatcher)))

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
  (setq consult-narrow-key (kbd "C-.")
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;; Actions on completion candidates
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
        modus-themes-mode-line nil))

(use-package nano-theme)

(use-package doom-themes
  :custom
  (doom-gruvbox-dark-variant "hard"))

;; Icons
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1))

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
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-maximum-width 60)
  (company-tooltip-minimum-width 60)
  (company-tooltip-align-annotations t)
  :config
  ;; Unbinding default insert mappings
  (general-define-key
   :states 'insert
   "C-j" nil
   "C-k" nil)
  (general-define-key
   :states 'insert
   :keymaps 'company-active-map
   "C-j"  'company-select-next
   "C-k"  'company-select-previous)
  (general-define-key
   :states 'insert
   :keymaps 'company-mode-map
   "C-SPC"  'company-complete))

(use-package company-box
  :custom
  (company-box-scrollbar nil)
  (company-box-doc-enable nil)
  :hook
  (company-mode . company-box-mode))

;; Setting font faces
(defun pada/set-fonts ()
  "Set the main font faces."
  (interactive)
  (set-face-attribute 'default nil :font pada/default-font-family :height pada/default-font-size :weight 'normal)
  (set-face-attribute 'fixed-pitch nil :font pada/default-font-family :weight 'normal)
  (set-face-attribute 'variable-pitch nil :font pada/variable-font-family :height pada/variable-font-size :weight 'light))

;; Frame parameters
(defvar pada/frame-parameters
  '((no-special-glyphs t)
    (internal-border-width 0)))

(setq frame-resize-pixelwise t)

(defun pada/set-frame-parameters ()
  "Set the parameters defined in `pada/frame-parameters' for the current frame."
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
        ("\\*\\(lsp-help\\|lsp-documentation\\)\\*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (side . bottom))
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

;; Mode line
(setq evil-mode-line-format '(before . mode-line-front-space))
(setq mode-line-defining-kbd-macro
      (propertize " Recording macro..." 'face 'mode-line-emphasis))

(defun pada/replace-vc-string (vc-string)
  "Replace VC-STRING with a simpler and more pleasent representation.
This function is meant to advise `vc-git-mode-line-string', particularly
as a `:filter-result' advice."
  (replace-regexp-in-string ".*Git[:-]" "" vc-string))

(advice-add 'vc-git-mode-line-string :filter-return 'pada/replace-vc-string)

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                "    "
                mode-line-position
                "    "
                (vc-mode vc-mode)
                "    "
                mode-line-modes
                "    "
                mode-line-misc-info
                mode-line-end-spaces))

(use-package minions
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '("" . ""))
  (minions-prominent-modes '(defining-kbd-macro))
  :init
  (minions-mode))


;; Mode line (the easy route)
;; (use-package doom-modeline
;;   :custom
;;   (doom-modeline-height 25)
;;   (doom-modeline-bar-width 4)
;;   (doom-modeline-minor-modes nil)
;;   (doom-modeline-indent-info nil)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-enable-word-count t)
;;   (doom-modeline-buffer-file-name-style 'relative-to-project)
;;   :init
;;   (doom-modeline-mode))

;; Time display format
(setq display-time-format "%A %d %b, %H:%M")
(setq display-time-default-load-average nil)

;; Org mode
(defun pada/org-mode-setup ()
  "Set options for `org-mode'. This function is meant to be added to `org-mode-hook'."
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode)
  (setq line-spacing 1)
  (flyspell-mode)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4))
  (org-latex-preview '(16))
  (general-define-key :states 'normal :keymaps 'org-mode-map "<tab>" 'evil-toggle-fold)
  (setq-local electric-pair-inhibit-predicate
              (lambda (c)
                (if (char-equal c ?<) t (electric-pair-default-inhibit c)))))

(use-package org
  :hook
  (org-mode . pada/org-mode-setup)
  :custom
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-return-follows-links t)
  (org-startup-with-inline-images t)
  ;; We set the preview in `pada/org-mode-setup', since we can't set the font scale before org starts
  (org-startup-with-latex-preview nil)
  (org-cycle-level-faces nil)
  (org-n-level-faces 4)
  (org-image-actual-width nil)
  (org-hidden-keywords '(title))
  (org-preview-latex-image-directory (expand-file-name "tmp/ltximg/" user-emacs-directory))
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")))
  (org-agenda-files '("~/org" "~/vault/Notes"))
  :config
  (add-to-list 'org-modules 'org-tempo)

  ;; Font scaling for different header levels
  (set-face-attribute 'org-level-8 nil :weight 'semi-bold :inherit 'default)
  (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.2)
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.44)
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.728)
  (set-face-attribute 'org-document-title nil :inherit 'org-level-8 :height 2.074)

  ;; Fonts that should always be in fixed-pitch
  (dolist (face '(org-block
                  org-block-begin-line
                  org-code
                  org-document-info-keyword
                  org-meta-line
                  org-table
                  org-verbatim
                  org-checkbox))
    (set-face-attribute `,face nil :inherit 'fixed-pitch))

  (set-face-attribute 'org-block-end-line nil :inherit 'org-block-begin-line)

  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "M-<tab>" 'org-shifttab
   "C-SPC" 'org-toggle-checkbox))

;; Toggle emphasis markers on cursor
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autokeywords t)
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

;; Toggle latex preview on cursor
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-cycle-headline-bullets nil)
  (org-superstar-headline-bullets-list '("◉" ("🞛" ?◈) "○" "▷"))
  (org-superstar-item-bullet-alist
   '((?* . ?•)
     (?+ . ?➤)
     (?- . ?•)))
  :config
  (set-face-attribute 'org-superstar-item nil :font pada/default-font-family :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :font pada/default-font-family :height 1.2))

;; Animate inline gifs source: https://ivanaf.com/animating_gifs_in_orgmode.html
(defun org-inline-image--get-current-image ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'org-image-overlay) t) (overlays-at (point)))))

(defun org-inline-image--get (prop)
  "Return the value of property PROP for image under point."
  (let ((image (org-inline-image--get-current-image)))
    (when image
      (overlay-get image prop))))

(defun org-inline-image-animate ()
  "Animate the image if it's possible."
  (interactive)
  (let ((image-props (org-inline-image--get 'display)))
    (when (image-multi-frame-p image-props)
      (image-animate image-props))))

(defun org-inline-image-animate-auto ()
  (interactive)
  (when (eq 'org-mode major-mode)
    (while-no-input
      (run-with-idle-timer 0.3 nil 'org-inline-image-animate))))

(setq org-inline-image--get-current-image (byte-compile 'org-inline-image--get-current-image))
(setq org-inline-image-animate  (byte-compile 'org-inline-image-animate ))
(add-hook 'post-command-hook 'org-inline-image-animate-auto)

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

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
  :hook
  (js-mode . add-node-modules-path)
  (typescript-mode . add-node-modules-path))

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
(use-package vterm)

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (js-mode . lsp)
  (typescript-mode . lsp)
  (tuareg-mode . lsp)
  (c-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (read-process-output-max (* 1024 1024))
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-signature-doc-lines 1)
  (lsp-restart 'iteractive)
  :config
  (general-define-key
   :states 'normal
   :keymaps 'lsp-mode-map
   "gd" 'lsp-find-definition
   "gr" '(lambda () (interactive) (lsp-find-references t)))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-signature-render-documentation t)
  (lsp-ui-doc-use-webkit t))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (flycheck-mode . flycheck-set-indication-mode)
  :custom
  (flycheck-display-errors-delay 0.6)
  (flycheck-idle-change-delay 0.01)
  (flycheck-check-syntax-automatically '(save idle-buffer-switch  idle-change mode-enabled))
  (flycheck-indication-mode 'left-margin))

;; Documentation in echo area
(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer nil)
  (eldoc-current-idle-delay 0.2))

;; Typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (before-save . lsp-eslint-apply-all-fixes)
  :custom
  (js-indent-level 2)
  (typescript-indent-level 2))

;; OCaml
(use-package tuareg)

(use-package mpdel
  :config
  (defun pada/mpdel-toggle-shuffle ()
    "Toggle mpd shuffle mode."
    (interactive)
    (if libmpdel--random
        (libmpdel-playback-unset-random)
      (libmpdel-playback-set-random)))

  (pada/leader-key
    "m" '(:ignore t :which-key "Mpdel")
    "mp" '(libmpdel-playback-play-pause :which-key "Toggle play")
    "ms" '(pada/mpdel-toggle-shuffle :which-key "Toggle shuffle")
    "mb" '(mpdel-browser-open :which-key "Browse")))

;; Spellcheck setup
(with-eval-after-load "ispell"
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "pt_BR,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pt_BR,en_US"))

;; Markdown setup
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

;; Display inline latex formulas and images
(use-package texfrag
  :hook
  (texfrag-mode . texfrag-document)
  (markdown-mode . texfrag-mode)
  (latex-mode . texfrag-mode))

;; Colorizing compilation buffer
(defun pada/colorize-compilation-buffer ()
  "Colorize compilation-buffer using `ansi-color'."
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'pada/colorize-compilation-buffer)

;;; init.el ends here
