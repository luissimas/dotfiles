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
(setq doom-font (font-spec :family "Iosevka Padawan" :size 14 :weight 'regular)
      doom-big-font (font-spec :family "Iosevka Padawan" :size 20 :weight 'regular)
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
(after! company
  (setq! company-box-scrollbar nil
         company-minimum-prefix-length 1
         copany-idle-delay 0.0)
  (map! :map company-mode-map
        :i "C-SPC" #'company-complete))

;; Formatting
(use-package! apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(prisma-mode prettier))

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
  :config
  (setq! lsp-auto-guess-root t
         lsp-signature-doc-lines 1
         lsp-lens-enable t
         lsp-elixir-suggest-specs nil
         lsp-elixir-dialyzer-enabled nil
         lsp-file-watch-threshold 5000)
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
  (setq! ispell-dictionary  "pt_BR,en_US")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "pt_BR,en_US"))

;; Ledger
(use-package! ledger-mode
  :mode "\\.journal\\'"
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
    ("nord" doom-nord)
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
            (consult-theme emacs-theme)
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


(defun pada/org-mode-setup ()
  "Set options for `org-mode'. This function is meant to be added to `org-mode-hook'."
  (mixed-pitch-mode)
  (visual-line-mode)
  (diff-hl-mode -1)
  (setq-local line-spacing 1
              display-line-numbers nil)
  (+zen/toggle))

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
  (set-face-attribute 'org-document-title nil :inherit 'org-level-8 :height 2.0)
  (set-face-attribute 'org-agenda-structure nil :weight 'semi-bold :height 1.4 :inherit 'default))

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
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Tasks on hold\n")))
            (agenda ""
                    ((org-agenda-span 1)
                     (org-deadline-warning-days 0)
                     (org-agenda-format-date "%A, %d %B %Y")
                     (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                     (org-agenda-overriding-header "\nDaily agenda\n")))
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
                        (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))))))

  (defun pada/custom-agenda (&optional arg)
    (interactive "P")
    (org-agenda arg "P"))

  (map! :leader
        :desc "Agenda"            "oaa" #'pada/custom-agenda
        :desc "Agenda dispatcher" "oaA"   #'org-agenda)

  ;; (add-to-list 'org-agenda-files "~/repos/zettelkasten")
  (add-hook! 'org-mode-hook :append #'pada/org-mode-setup)
  (add-hook 'org-agenda-mode-hook #'hide-mode-line-mode)
  (add-hook 'doom-load-theme-hook #'pada/set-org-faces)
  (advice-add #'consult-theme :after (lambda (&rest args) (pada/set-org-faces))))

(use-package! org-habit
  :after org
  :config
  (setq org-habit-graph-column 50
        org-habit-preceding-days 7
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
  '(("^\\*Alchemist-IEx\\*" :quit nil :size 0.3)))

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
         'action
         (lambda (_) (+doom-dashboard-reload t))
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
          :map org-mode-map
          :desc "Open graph" "mmg" #'org-roam-ui-open))

(use-package! elfeed
  :config
  (add-hook! 'elfeed-search-mode-hook 'elfeed-update)
  (setq! elfeed-goodies/entry-pane-size 0.5
         elfeed-goodies/feed-source-column-width 20
         elfeed-goodies/tag-column-width 30
         elfeed-search-filter "@6-months-ago")
  (setq elfeed-feeds
        '(("https://www.youtube.com/feeds/videos.xml?channel_id=UCYCO3Kifwg56zhus3XXiAVg" youtube productivity)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcaTUtGzOiS4cqrgtcsHYWg" youtube productivity)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" youtube programming)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOJNw9aHGRkYuIOqwU7yK-Q" youtube travel)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" youtube programming)
          ("https://protesilaos.com/codelog.xml" programming)
          ("https://phaazon.net/blog/feed" programming)
          ("https://protesilaos.com/commentary.xml" misc)
          ("https://protesilaos.com/news.xml" misc)
          ("https://lukesmith.xyz/index.xml" misc)
          ("https://curiosum.dev/blog/rss.xml" programming))))

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
