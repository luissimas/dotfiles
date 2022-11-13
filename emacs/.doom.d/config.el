;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Luís Simas"
      user-mail-address "luissimas@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Iosevka Padawan" :size 13 :weight 'regular)
      doom-big-font (font-spec :family "Iosevka Padawan" :size 24 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(map! :leader
      :desc "M-x" "x" #'execute-extended-command)

(map! :nv "gr" #'+lookup/references)

;; Disabling window-divider-mode
(remove-hook 'doom-init-ui-hook #'window-divider-mode)

;; Disabling hl-line-mode
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Disabling mode-line on dashboard
(add-hook! +doom-dashboard-mode #'hide-mode-line-mode)

(after! evil
  (setq +evil-want-o/O-to-continue-comments nil
        evil-want-minibuffer t
        evil-move-cursor-back nil
        evil-shift-width 2)
  (map! :nv "H" 'evil-beginning-of-line
        :nv "L" 'evil-end-of-line))

;; Unique buffer name formats
;; doom's `persp-mode' activation disables uniquify, b/c it says it breaks it.
;; It doesn't cause big enough problems for me to worry about it, so we override
;; the override. `persp-mode' is activated in the `doom-init-ui-hook', so we add
;; another hook at the end of the list of hooks to set our uniquify values.
(add-hook! 'doom-init-ui-hook
           :append ;; ensure it gets added to the end.
           #'(lambda () (require 'uniquify) (setq uniquify-buffer-name-style 'forward)))

;; Fringe width
(after! git-gutter-fringe
  (set-fringe-mode '(2 . 0)))

;; Treemacs icon theme
(setq! doom-themes-treemacs-theme "doom-colors")

;; Scroll offset
(setq! scroll-margin 8)

;; Tab width
(setq! tab-width 2
       standard-indent 2
       backward-delete-char-untabify-method 'hungry)

;; Treesitter
(use-package! tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode))

;; Projectile
(after! projectile
  (setq! projectile-project-search-path '(("~/fun" . 3) "~/liven" ("~/cati" . 2))
         projectile-enable-caching nil
         projectile-switch-project-action #'projectile-dired
         projectile-indexing-method 'hybrid))

;; Persp-mode
(map! :leader :map doom-leader-workspace-map
      (:prefix-map ("TAB" . "workspace")
       :desc "Display tab bar"           "TAB" #'+workspace/display
       :desc "Switch workspace"          "s"   #'+workspace/switch-to
       :desc "Switch to last workspace"  "l"   #'+workspace/other
       :desc "New workspace"             "n"   #'+workspace/new-named
       :desc "New unnamed workspace"     "N"   #'+workspace/new
       :desc "Load workspace from file"  "L"   #'+workspace/load
       :desc "Save workspace to file"    "S"   #'+workspace/save
       :desc "Delete session"            "x"   #'+workspace/kill-session
       :desc "Delete this workspace"     "k"   #'+workspace/delete
       :desc "Rename workspace"          "r"   #'+workspace/rename
       :desc "Restore last session"      "R"   #'+workspace/restore-last-session))

;; Company
(after! company
  (setq! company-box-scrollbar nil
         company-minimum-prefix-length 1
         copany-idle-delay 0.0)
  (map! :map company-mode-map
        :i "C-SPC" #'company-complete))

;; Formatting
(apheleia-global-mode)

;; LSP
(after! lsp-mode
  ;; Disable creation on ts-server .log files
  (setq! lsp-auto-guess-root t
         lsp-signature-doc-lines 1
         lsp-ui-sideline-enable nil
         lsp-lens-enable t
         lsp-elixir-suggest-specs nil
         lsp-elixir-dialyzer-enabled nil))
         

;; Flycheck
(after! flycheck-credo
  (setq! flycheck-elixir-credo-strict t))

;; Magit
(after! magit
  (setq! magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show))

;; Vertico
(after! vertico
  (setq! vertico-count 10)
  (map! :map vertico-map
        :n "<escape>" #'abort-minibuffers))

;; Consult
(after! consult
  (map! :n "C-s" #'consult-line)
  (map! :leader
        :desc "Grep" "fg" #'consult-ripgrep))

;; Orderless
(after! orderless
  (setq! orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))

;; Themes
(after! modus-themes
  (setq! modus-themes-subtle-line-numbers t
         modus-themes-mode-line '(borderless)
         modus-themes-org-blocks 'gray-background
         modus-themes-hl-line '(accented)))

(after! ispell
  (setq! ispell-dictionary  "pt_BR,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pt_BR,en_US"))

;; Ledger
(use-package! ledger-mode
  :mode "\\.journal\\'"
  :hook (ledger-mode . (lambda ()
                         (add-hook 'before-save-hook 'ledger-mode-clean-buffer nil 'make-it-local)))
  :config
  (setq! ledger-post-amount-alignment-column 60))

(use-package! hledger-mode
  :config
  (setq! hledger-jfile (expand-file-name "~/dox/accounting/accounting.journal")
         hledger-reporting-day 1)
  (map! :map hledger-mode-map
        :localleader
        :desc "Hledger report" "mr"  #'hledger-run-command))

;; Open files in external programs
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

;; Sync system theme with emacs theme
(defcustom pada/system-theme-associations
  '(("modus-operandi" modus-operandi)
    ("modus-vivendi" modus-vivendi)
    ("nord" doom-nord)
    ("gruvbox" doom-gruvbox)
    ("tokyonight" doom-tokyo-night)
    ("palenight" doom-palenight)
    ("pywal" ewal-doom-one)
    ("catppuccin" catppuccin))
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
            (consult-theme emacs-theme)
            (setq associations nil)))))))

;; Load system theme on startup
(add-hook 'emacs-startup-hook 'pada/load-system-theme)

;; Macro recording display format
(setq mode-line-defining-kbd-macro
      (propertize " Recording macro..." 'face 'mode-line-emphasis))

(defun pada/replace-vc-string (vc-string)
  "Replace VC-STRING with a simpler and more pleasent representation.
This function is meant to advise `vc-git-mode-line-string', particularly
as a `:filter-result' advice."
  (replace-regexp-in-string ".*Git[:-]" "" vc-string))

(advice-add 'vc-git-mode-line-string :filter-return 'pada/replace-vc-string)

(setq column-number-mode 1
      line-number-mode 1)

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

(use-package! minions
  :custom
  (minions-mode-line-lighter "")
  (minions-mode-line-delimiters '("" . ""))
  (minions-prominent-modes '(defining-kbd-macro))
  :init
  (minions-mode))

;; Time display format
(setq display-time-format "%A %d %b, %H:%M")
(setq display-time-default-load-average nil)

;; Remove truncation and continuation indicators
(setq-default fringe-indicator-alist
              (assq-delete-all 'continuation
                               (assq-delete-all 'truncation fringe-indicator-alist)))

(defun pada/set-frame-parameters ()
  "Set custom parameters for the current frame."
  (interactive)
  (dolist (parameter '((no-special-glyphs t)
                       (internal-border-width 0)))
    (set-frame-parameter (selected-frame) (car parameter) (car (cdr parameter)))))

;; Setting frame options in both daemon (with hooks) or on
;; normal emacs startup (directly calling the functions)
(if (daemonp)
    (progn
      (add-hook 'server-after-make-frame-hook 'pada/set-frame-parameters))
  (progn
    (pada/set-frame-parameters)))

(use-package! vterm
  :config
  (map! :map 'vterm-mode-map
        :i "C-c"   #'vterm--self-insert
        :i "C-d"   #'vterm--self-insert
        :i "C-SPC" #'vterm--self-insert))


(defun pada/org-mode-setup ()
  "Set options for `org-mode'. This function is meant to be added to `org-mode-hook'."
  (mixed-pitch-mode)
  (visual-line-mode)
  (git-gutter-mode -1)
  (setq-local line-spacing 1
              display-line-numbers nil))

(defun pada/set-org-faces ()
  "Customize faces for `org-mode' headings.
This function is meant to be added to `doom-load-theme-hook' and to advice after `consult-theme'."
  (set-face-attribute 'org-level-8 nil :weight 'semi-bold :inherit 'default)
  (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.1)
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.2)
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.3)
  (set-face-attribute 'org-document-title nil :inherit 'org-level-8 :height 2.0))

(use-package! org
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-tag-alist '(("ufscar") ("liven") ("personal"))
        org-format-latex-options (plist-put org-format-latex-options :scale 1.1)
        org-hidden-keywords '(title)
        org-deadline-warning-days 5
        org-agenda-start-with-log-mode t
        org-agenda-tags-column 10
        org-log-done 'timer
        org-log-into-drawer t)

  (add-to-list 'org-agenda-files "~/dox/vault/Notes")
  (add-hook 'org-mode-hook #'pada/org-mode-setup)
  (add-hook 'doom-load-theme-hook #'pada/set-org-faces)
  (advice-add #'consult-theme :after (lambda (&rest args) (pada/set-org-faces))))

(use-package! org-habit
  :after org)

(use-package! org-modern
  :after org
  :config
  (setq org-modern-list '((?* . "•")
                          (?+ . "◦")
                          (?- . "•"))
        org-modern-star '("◉ " "🞛 " "○ " "◇ "))
  (set-face-attribute 'org-modern-symbol nil :family "Iosevka" :height 1.2)
  (set-face-attribute 'org-modern-label nil :height 1.0)
  :init (global-org-modern-mode))

;; Toggle emphasis markers on cursor
(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoentities t
       org-appear-autokeywords t
       org-appear-autolinks t
       org-appear-autosubmarkers t))

;; Toggle latex preview on cursor
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package! org-journal
  :config
  (setq org-journal-date-prefix "#+title: "
        org-journal-time-prefix "* "
        org-journal-date-format "%a, %d-%m-%Y"
        org-journal-file-format "%Y-%m-%d.org"))

(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))
