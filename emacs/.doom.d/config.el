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
(setq doom-font (font-spec :family "Iosevka Padawan" :size 18 :weight 'regular)
      doom-big-font (font-spec :family "Iosevka Padawan" :size 25 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 14))
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
(setq org-directory "~/docs/org/"
      org-roam-directory "~/repos/zettelkasten")

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

;; Use trash
(setq delete-by-moving-to-trash t)

;; Disabling window-divider-mode
(remove-hook 'doom-init-ui-hook #'window-divider-mode)

;; Disabling hl-line-mode
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Keybinding to toggle modeline
(map! :leader
        :desc "Modeline" "tm" #'hide-mode-line-mode)

(add-hook! 'compilation-mode-hook #'hide-mode-line-mode)

(use-package! evil
  :config
  (setq +evil-want-o/O-to-continue-comments nil
        evil-want-minibuffer t
        evil-move-cursor-back nil
        evil-shift-width 2)
  (map! :leader
        :desc "M-x" "x" #'execute-extended-command)

  (map! :nv "gr" #'+lookup/references)

  (map! :nv "H" 'evil-beginning-of-line
        :nv "L" 'evil-end-of-line
        :nv "j" 'evil-next-visual-line
        :nv "k" 'evil-previous-visual-line))

;; Unique buffer name formats
;; doom's `persp-mode' activation disables uniquify, b/c it says it breaks it.
;; It doesn't cause big enough problems for me to worry about it, so we override
;; the override. `persp-mode' is activated in the `doom-init-ui-hook', so we add
;; another hook at the end of the list of hooks to set our uniquify values.
(add-hook! 'doom-init-ui-hook
           :append ;; ensure it gets added to the end.
           #'(lambda () (require 'uniquify) (setq uniquify-buffer-name-style 'forward)))

;; Treemacs icon theme
(setq! doom-themes-treemacs-theme "doom-colors")

(setq! doom-gruvbox-dark-variant "hard")

;; Scroll offset
(setq! scroll-margin 8)

;; Tab width
(setq! tab-width 2
       standard-indent 2
       backward-delete-char-untabify-method 'hungry)

;; Projectile
(after! projectile
  (setq! projectile-project-search-path '(("~/fun" . 4) "~/liven" ("~/cati" . 2) ("~/docs" . 2) ("~/freela". 3))
         projectile-enable-caching nil
         projectile-per-project-compilation-buffer t
         projectile-indexing-method 'hybrid))

;; Persp-mode
(after! persp-mode
  (setq +workspaces-on-switch-project-behavior t
        +workspaces-switch-project-function #'magit-status
        persp-auto-resume-time 0
        persp-init-frame-behaviour t
        persp-interactive-init-frame-behaviour-override -1
        persp-emacsclient-init-frame-behaviour-override -1)
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
         :desc "Previous workspace"        "i"   #'+workspace/switch-left
         :desc "Next workspace"            "o"   #'+workspace/switch-right
         :desc "Rename workspace"          "r"   #'+workspace/rename
         :desc "Restore last session"      "R"   #'+workspace/restore-last-session)))

;; Company
;; (after! company
;;   (setq! company-box-scrollbar nil
;;          company-minimum-prefix-length 1
;;          copany-idle-delay 0.0)
;;   (map! :map company-mode-map
;;         :i "C-SPC" #'company-complete))

;; Treemacs
(use-package! treemacs
  :config
  (treemacs-follow-mode))

;; Formatting
(use-package! apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(prisma-mode prettier))

  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "-"))
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))

  ;; By default Apheleia runs commands in the buffer cwd, this advice makes it
  ;; run the commands in the current project root. This is important to make mix
  ;; formatter respect the project configuration in .formatter.exs.
  ;; source: https://github.com/radian-software/apheleia/issues/30#issuecomment-778150037
  (defun pada/fix-apheleia-project-dir (orig-fn &rest args)
    (let ((project (project-current)))
      (if (not (null project))
          (let ((default-directory (project-root project))) (apply orig-fn args))
        (apply orig-fn args))))

  (advice-add 'apheleia-format-buffer :around #'pada/fix-apheleia-project-dir)

  :init (apheleia-global-mode))

;; LSP
(use-package! lsp-mode
  :init
  (setq! lsp-pyright-multi-root nil)
  :config
  (setq! lsp-auto-guess-root t
         lsp-signature-doc-lines 1
         lsp-lens-enable t
         lsp-completion-provider :none
         lsp-elixir-suggest-specs nil
         lsp-elixir-dialyzer-enabled nil
         lsp-file-watch-threshold 5000)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]volumes\\'")
  ;; (add-to-list 'lsp-language-id-configuration '(elixir-mode . "elixir-lexical"))
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection "/home/padawan/repos/lexical/_build/dev/rel/lexical/start_lexical.sh")
  ;;                   :activation-fn (lsp-activate-on "elixir-lexical")
  ;;                   :server-id 'elixir-lexical))
  (add-hook! 'prisma-mode-hook #'lsp))

(use-package! lsp-ui
  :config
  (setq! lsp-ui-doc-enable nil
         lsp-ui-doc-header nil
         lsp-ui-doc-include-signature t
         lsp-ui-doc-delay 0
         lsp-ui-doc-max-height 50
         lsp-ui-doc-position 'at-point
         lsp-ui-peek-enable nil
         lsp-ui-imenu-enable nil
         lsp-ui-sideline-enable nil))

;; Flycheck
(use-package! flycheck
  :hook
  (flycheck-mode . (lambda () (flycheck-set-indication-mode 'left-margin)))
  :config
  (setq! flycheck-display-errors-delay 0.2))

(use-package! flycheck-credo
  :config
  (setq! flycheck-elixir-credo-strict t))

;; Eldoc
(use-package! eldoc
  :config
  (setq! eldoc-idle-delay 0.1))

;; Magit
(after! magit
  (setq! magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
         magit-save-repository-buffers 'dontask
         magit-list-refs-sortby "-creatordate")
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show))

;; Vertico
(after! vertico
  (setq! vertico-count 10)
  (map! :map vertico-map
        :n "<escape>" #'abort-minibuffers)
  (map! :leader
        "'" nil ;; Removing the default mapping
        :desc "Vertico repeat" "." #'vertico-repeat))

;; Consult
(after! consult
  (map! :n "C-s" #'consult-line)
  (map! :leader
        :desc "Grep" "fg" #'consult-ripgrep))

(use-package! consult-lsp
  :config
  (map! :leader
        :map lsp-mode-map
        :desc "Diagnostics" "cd" #'consult-lsp-diagnostics))

;; Orderless
(use-package! orderless
  :config
  (setq! completion-styles '(orderless basic)
         completion-ignore-case t
         completion-category-defaults nil
         read-file-name-completion-ignore-case t
         read-buffer-completion-ignore-case t
         orderless-matching-styles '(orderless-literal orderless-flex orderless-regexp)))

;; Themes
(after! modus-themes
  (setq! modus-themes-subtle-line-numbers t
         modus-themes-mode-line '(borderless)
         modus-themes-org-blocks 'gray-background
         modus-themes-hl-line '(accented)))

(use-package nano-theme)

(after! ispell
  (setq! ispell-dictionary  "pt_BR,en_US"
         ispell-silently-savep t)
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pt_BR,en_US"))

;; Ledger
(use-package! ledger-mode
  :mode "\\.journal\\'"
  :demand t
  :hook
  (ledger-mode . (lambda () (add-hook 'before-save-hook 'ledger-mode-clean-buffer nil 'make-it-local)))
  :config
  (map! :leader
        :desc "Ledger report" "ol" #'ledger-report)

  (setq! ledger-post-amount-alignment-column 60
         ledger-mode-should-check-version nil
         ledger-binary-path "hledger")

  (add-hook! ledger-report-mode #'hl-line-mode)

  (setq ledger-reports
        '(("Balance sheet" "hledger balancesheet --drop=1")
          ("Income statement current month" "hledger is -p thismonth -S --drop=1")
          ("Income statement past 6 months" "hledger is -M -b '6 months ago' -SA --drop=1")
          ("Expenses report" "hledger bal expenses -M -b '6 months ago' -SA --depth=2 --drop=1")
          ("Investments report" "hledger bal assets:investments --no-total --drop=2"))))

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
    ("nord" doom-nord-aurora)
    ("gruvbox" doom-gruvbox)
    ("tokyonight" doom-tokyo-night)
    ("palenight" doom-palenight)
    ("pywal" ewal-doom-one)
    ("onedark" doom-one)
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
            (setq doom-theme emacs-theme)
            (load-theme emacs-theme t)
            (setq associations nil)))))))

;; Load system theme on startup
(add-hook 'doom-after-init-hook #'pada/load-system-theme)

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
      (add-hook! 'server-after-make-frame-hook #'pada/set-frame-parameters (enable-theme doom-theme)))
  (progn
    (pada/set-frame-parameters)))

(use-package! vterm
  :config
  (map! :map 'vterm-mode-map
        :i "C-c"   #'vterm--self-insert
        :i "C-d"   #'vterm--self-insert
        :i "C-SPC" #'vterm--self-insert))

(use-package! org
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-preview-latex-default-process 'imagemagick
        org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-tag-alist '(("ufscar") ("liven") ("personal"))
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-hidden-keywords '(title)
        org-deadline-warning-days 5
        org-agenda-start-with-log-mode t
        org-tags-column 0
        org-agenda-span 'week
        org-agenda-start-day "-0d"
        org-agenda-start-on-weekday 0
        org-scheduled-past-days 0
        org-agenda-block-separator ?—
        org-agenda-time-leading-zero t
        org-agenda-current-time-string (concat "Now " (make-string 70 ?-))
        org-agenda-tags-column 10
        org-log-done 'timer
        org-log-into-drawer t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  ;; Better code blocks highlighting on latex export
  (setq org-latex-listings 'engraved)

  ;; Tikz to draw graphics
  (setq org-latex-packages-alist (append org-latex-packages-alist '(("" "tikz" t)
                                                                    ("" "qtree" t)
                                                                    ("" "forest" t)
                                                                    "\\usetikzlibrary{arrows,automata,positioning}")))

  (setq org-agenda-custom-commands
        '(("P" "Padawan's custom agenda"
           ((todo ""
                  ((org-agenda-overriding-header "Unscheduled tasks\n")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))
            (agenda ""
                    ((org-agenda-span 1)
                     (org-deadline-warning-days 0)
                     (org-agenda-format-date "%A, %d %B %Y")
                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                     (org-agenda-overriding-header "\nDaily agenda\n")
                     (org-agenda-skip-function #'pada/org-agenda-skip-habits)))
            (agenda ""
                    ((org-agenda-span 3)
                     (org-agenda-start-on-weekday nil)
                     (org-agenda-start-day "+1d")
                     (org-deadline-warning-days 0)
                     ;; (org-agenda-show-all-dates nil)
                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-overriding-header "\nNext three days\n")))
            (agenda "" ((org-agenda-time-grid nil)
                        (org-agenda-start-on-weekday nil)
                        (org-agenda-start-day "+4d")
                        (org-agenda-span 14)
                        (org-agenda-show-all-dates nil)
                        (org-deadline-warning-days 0)
                        (org-agenda-entry-types '(:deadline))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                        (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
            (agenda "" ((org-agenda-span 1)
                        (org-agenda-time-grid nil)
                        (org-agenda-prefix-format "")
                        (org-agenda-format-date "")
                        (org-agenda-skip-function #'pada/org-agenda-skip-non-habits)
                        (org-agenda-overriding-header "\nHabits")))))))

  (defun pada/org-agenda-skip-habits ()
    "Skip entries with STYLE property set to 'habit'."
    (org-back-to-heading t)
    (let ((style (org-entry-get (point) "STYLE")))
      (if (string-equal style "habit")
          (progn
            (org-next-visible-heading 1)
            (point)))))

  (defun pada/org-agenda-skip-non-habits ()
    "Skip entries where the STYLE property is no set to 'habit'."
    (org-back-to-heading t)
    (let ((style (org-entry-get (point) "STYLE")))
      (unless (string-equal style "habit")
          (progn
            (org-next-visible-heading 1)
            (point)))))

  (defun pada/custom-agenda (&optional arg)
    (interactive "P")
    (org-agenda arg "P"))

  (map! :leader
        :desc "Agenda"            "oaa" #'pada/custom-agenda
        :desc "Agenda dispatcher" "oaA"   #'org-agenda)

  (defun pada/org-mode-setup ()
    "Set options for `org-mode'. This function is meant to be added to `org-mode-hook'."
    (mixed-pitch-mode)
    (visual-line-mode)
    (diff-hl-mode -1)
    (setq-local line-spacing 1
                display-line-numbers nil)
    (+zen/toggle))

  ;; (add-to-list 'org-agenda-files "~/repos/zettelkasten")
  (add-hook! 'org-mode-hook :append #'pada/org-mode-setup))

(use-package! org-habit
  :after org
  :config
  (remove-hook 'org-agenda-mode-hook #'+org-habit-resize-graph-h)
  (setq org-habit-following-days 4
        org-habit-preceding-days 20
        org-habit-graph-column 25
        org-habit-show-all-today t
        org-habit-show-done-always-green t))

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
       org-appear-autolinks nil
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

;; Popup rules
(set-popup-rules!
  '(("\\*\\([Hh]elp.*\\|info\\)\\*" :side right :width 0.4 :slot 0 :ttl 0 :quit current))
  '(("^\\*Alchemist-IEx\\*" :quit nil :size 0.3))
  '(("^+new-snippet+" :quit nil :size 0.3))
  '(("^\\*eww\\*" :side right :size 0.5 :quit nil :select t))
  '(("^\\*doom:\\(?:v?term\\|e?shell\\)-popup"
     :vslot -5 :size 0.3 :select t :modeline nil :quit nil :ttl nil)))

;; Disabling mode-line on dashboard
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1))

;; Hiding cursor on dashboard
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

;; Random quote on dashboard
(defun pada/doom-dashboard-quote ()
  "Get a random quote and format it to display on Doom's dashboard."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         ;; 'action
         ;; (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-desc
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (s-trim (shell-command-to-string "quote.sh")))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defun pada/doom-dashboard-quote-widget ()
  (setq line-spacing 0.2)
  (insert "\n\n" (pada/doom-dashboard-quote) "\n"))

(setq! +doom-dashboard-functions '(doom-dashboard-widget-banner
                                   pada/doom-dashboard-quote-widget
                                   doom-dashboard-widget-loaded))

(setq fancy-splash-image (expand-file-name "icon.png" doom-user-dir))

;; Org-roam
(use-package! websocket
    :after org-roam)

(defun browse-url-surf (url &optional _new-window)
  "Ask the surf WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (message url)
  (start-process (concat "surf " url) nil "surf" url))

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t
          org-roam-ui-browser-function #'browse-url-surf)
    (map! :leader
          :desc "Open graph" "nrg" #'org-roam-ui-open)
    (map! :leader
          :map org-roam-mode-map
          :desc "Open graph" "mmg" #'org-roam-ui-open))

(defun pada/elfeed-show-mode-setup ()
  "Set options for `elfeed-show-mode'. This function is meant to be added to `elfeed-show-mode-hook'."
  (mixed-pitch-mode)
  (setq-local line-spacing 1
              display-line-numbers nil
              scroll-margin 0)
  (setq-local evil-normal-state-cursor nil
              evil-visual-state-cursor nil
              cursor-type nil)
  (centered-cursor-mode)
  (focus-mode)
  (+zen/toggle))

(use-package! centered-cursor-mode
  :after elfeed)

(use-package! focus
  :after elfeed
  :config
  (add-to-list 'focus-mode-to-thing '(elfeed-show-mode . paragraph)))

(use-package! elfeed
  :config
  (add-hook! 'elfeed-search-mode-hook #'elfeed-update)
  (add-hook! 'elfeed-search-mode-hook #'centered-cursor-mode)
  (add-hook! 'elfeed-show-mode-hook #'pada/elfeed-show-mode-setup)
  (map! :map 'elfeed-show-mode-map
        :n "j" #'evil-forward-paragraph
        :n "k" #'evil-backward-paragraph)
  (setq! elfeed-goodies/entry-pane-size 0.5
         elfeed-goodies/feed-source-column-width 20
         elfeed-goodies/tag-column-width 30
         elfeed-search-filter "@6-months-ago")
  (setq elfeed-feeds
        '(("https://www.youtube.com/feeds/videos.xml?channel_id=UCYCO3Kifwg56zhus3XXiAVg" youtube productivity)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcaTUtGzOiS4cqrgtcsHYWg" youtube productivity)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" youtube programming)
          ("https://protesilaos.com/codelog.xml" programming)
          ("https://protesilaos.com/commentary.xml" misc)
          ("https://protesilaos.com/news.xml" misc)
          ("https://phaazon.net/blog/feed" programming)
          ("https://lukesmith.xyz/index.xml" misc))))

(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup))

(use-package! elfeed-tube-mpv
  :after elfeed-tube)

(use-package! doom-modeline
  :config
  (setq! doom-modeline-buffer-file-name-style 'buffer-name
         doom-modeline-major-mode-icon nil
         doom-modeline-major-mode-color-icon t
         doom-modeline-enable-word-count t
         doom-modeline-checker-simple-format t
         doom-modeline-vcs-max-length 20
         doom-modeline-lsp nil)
  (setq! lsp-modeline-diagnostics-enable nil)
  (setq all-the-icons-scale-factor 1.0))

(use-package! modus-themes
  :init
  (setq! modus-themes-org-blocks 'gray-background
         modus-themes-common-palette-overrides
         '((fg-line-number-inactive "gray50")
           (fg-line-number-active fg-main)
           (bg-line-number-inactive unspecified)
           (bg-line-number-active unspecified)
           (border-mode-line-active unspecified)
           (border-mode-line-inactive unspecified)
           (fringe unspecified))))

(use-package! diff-hl
  :config
  (setq diff-hl-draw-borders nil))

(use-package! typescript-mode
  :config
  (setq! js-indent-level 2
         typescript-indent-level 2))

(use-package! inf-elixir
  :demand t
  :config
  (defun pada/elixir-project-p ()
    "Check if the current projectile project is and Elixir project."
    (interactive)
    (and (projectile-project-p) (file-exists-p (concat (projectile-project-root) "mix.exs"))))

  (defun +elixir/open-repl ()
    "Open Elixir REPL."
    (interactive)
    (pop-to-buffer
     (if (pada/elixir-project-p)
         (inf-elixir-project)
       (inf-elixir))))

  (set-repl-handler! 'elixir-mode #'+elixir/open-repl))

(defun pada/capitalize-first-char (&optional string)
  "Capitalize the first character of STRING."
  (when (and string (> (length string) 0))
    (let ((first-char (substring string nil 1))
          (rest-str   (substring string 1)))
      (concat (capitalize first-char) rest-str))))

(defun yas-new-snippet (&optional no-template)
  "Pops a new buffer for writing a snippet.

Expands a snippet-writing snippet, unless the optional prefix arg
NO-TEMPLATE is non-nil."
  (interactive "P")
  (let ((guessed-directories (yas--guess-snippet-directories))
        (yas-selected-text (or yas-selected-text
                               (and (region-active-p)
                                    (buffer-substring-no-properties
                                     (region-beginning) (region-end))))))

    (pop-to-buffer yas-new-snippet-buffer-name)
    (erase-buffer)
    (kill-all-local-variables)
    (snippet-mode)
    (yas-minor-mode 1)
    (set (make-local-variable 'yas--guessed-modes)
         (mapcar (lambda (d) (yas--table-mode (car d)))
                 guessed-directories))
    (set (make-local-variable 'default-directory)
         (car (cdr (car guessed-directories))))
    (if (and (not no-template) yas-new-snippet-default)
        (yas-expand-snippet yas-new-snippet-default))))

;; Corfu
(use-package! corfu
  :init (global-corfu-mode)
  :config
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-separator ?\s
        corfu-preview-current nil
        corfu-quit-no-match 'separator
        corfu-on-exact-match nil
        corfu-bar-width 0
        corfu-min-width 80
        corfu-max-width 100
        corfu-scroll-margin 4
        corfu-echo-delay 0.2
        corfu-popupinfo-delay (cons nil 0.2)
        corfu-popupinfo-hide nil
        tab-always-indent 'complete)

  (defun pada/+org-return-advice (fun &rest args)
    "Advice for `+org/return' that inserts the selected completion candidate if it exists."
    (if (and (boundp 'corfu-mode) (>= corfu--index 0))
        (corfu-insert)
      (apply fun args)))

  (advice-add #'+org/return :around #'pada/+org-return-advice)

  (add-hook! 'global-corfu-mode-hook
             #'corfu-history-mode
             #'corfu-echo-mode)

  (defun pada/corfu-quit ()
    "Quits corfu completion and enter evil normal mode."
    (interactive)
    (corfu-quit)
    (evil-normal-state))

  (map! :mode global-corfu-mode :map corfu-mode-map :i "C-SPC" #'completion-at-point)
  (map! :mode global-corfu-mode :map corfu-map
        :i
        [escape] #'pada/corfu-quit
        "ESC" #'pada/corfu-quit
        "C-SPC" #'corfu-quit
        "C-j" #'corfu-next
        "C-k" #'corfu-previous
        "C-h" #'corfu-info-documentation
        "C-l" #'corfu-info-location)

  (defun pada/lsp-corfu-setup ()
    "Setup corfu completion style for lsp."
    (setq-local completion-styles '(orderless)
                completion-category-defaults nil))

  (add-hook! 'lsp-completion-mode-hook #'pada/lsp-corfu-setup)

  (defun pada/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'pada/corfu-enable-in-minibuffer)
  (add-hook 'eshell-mode-hook #'corfu-mode))

(use-package! company
  :after corfu)

(use-package! kind-icon
  :after corfu
  :config
  (setq kind-icon-default-face 'corfu-default ; to compute blended backgrounds correctly
        kind-icon-blend-background nil)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'doom-load-theme-hook #'kind-icon-reset-cache))

(use-package! cape
  :init
  (defun pada/cape-capf-setup-git-commit ()
    (add-to-list 'completion-at-point-functions
                 (cape-super-capf #'cape-dabbrev #'cape-ispell #'cape-symbol #'cape-file #'cape-tex)))

  (defun pada/cape-capf-setup-org ()
    (require 'org-roam)
        (add-to-list 'completion-at-point-functions
                     (apply #'cape-super-capf
                            (append
                             (if (org-roam-file-p) org-roam-completion-functions ())
                             (list #'cape-ispell #'cape-dabbrev (cape-company-to-capf #'company-yasnippet) #'cape-file #'cape-tex)))))

  (defun pada/cape-capf-setup-lsp ()
    (add-to-list 'completion-at-point-functions
                 (cape-super-capf #'lsp-completion-at-point (cape-company-to-capf #'company-yasnippet) #'cape-file #'cape-ispell #'cape-dabbrev)))


  (add-hook! 'lsp-completion-mode-hook #'pada/cape-capf-setup-lsp)
  (add-hook! 'git-commit-mode-hook #'pada/cape-capf-setup-git-commit)
  (add-hook! 'org-mode-hook #'pada/cape-capf-setup-org))

;; Marginalia
(use-package! marginalia
  :config
  (map! :map minibuffer-local-map
        "M-a" #'marginalia-cycle))

(use-package! poetry
  :config

  (defvar pada/poetry--last-project-venv nil
    "Used to store the last value of `poetry-project-venv' set on `pada/set-pyright-venv'.")

  (defun pada/set-pyright-venv ()
    "Set `lsp-pyright-venv-path' the same as `poetry-project-venv'.
Also restarts the LSP workspace via `lsp-workspace-restart' so the
venv change affects pyright."

    (if (and (boundp 'lsp-pyright-venv-path)
             (boundp 'poetry-project-venv)
             poetry-project-venv
             (not (string-equal pada/poetry--last-project-venv poetry-project-venv)))
        (progn
          (message "Setting lsp-pyright-venv-path")
          (setq pada/poetry--last-project-venv poetry-project-venv)
          (setq-local lsp-pyright-venv-path poetry-project-venv)
          (if lsp-mode
              (progn
                (lsp-workspace-restart (car (lsp-workspaces))))))))

  (add-hook 'lsp-mode-hook #'pada/set-pyright-venv))
