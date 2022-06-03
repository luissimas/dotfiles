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

;; doom-plain doom-moonlight doom-tomorrow-night
(setq doom-theme 'doom-tomorrow-night)

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

(after! evil
  (setq +evil-want-o/O-to-continue-comments nil
        evil-want-minibuffer t
        evil-shift-width 2)
  (map! :n "H" 'evil-beginning-of-line
        :n "L" 'evil-end-of-line))

;; Unique buffer name formats
(setq! uniquify-buffer-name-style 'forward)

;; Fringe width
(after! git-gutter-fringe
  (set-fringe-mode '(1 . 0)))

;; Removing def from prettify-symbols
(plist-delete! +ligatures-extra-symbols :def)
(plist-delete! +ligatures-extra-symbols :not)

;; Scroll offset
(setq! scroll-margin 8)

;; Tab width
(setq! tab-width 2
       standard-indent 2
       backward-delete-char-untabify-method 'hungry)

;; Treesitter
(use-package! tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Projectile
(after! projectile
  (setq! projectile-project-search-path '(("~/fun" . 3) "~/liven" ("~/cati" . 2))
         projectile-enable-caching nil
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

;; Disabling hl-line-mode
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; Company
(after! company
  (setq company-box-scrollbar nil)
  (map! :map company-mode-map
        :i "C-SPC" #'company-complete))

;; Formatting
(setq! +format-on-save-enabled-modes '(not tex-mode
                                           latex-mode
                                           org-msg-edit-mode))
(after! format-all
  (setq! format-all-show-errors 'never
         +format-with-lsp nil))

;; LSP
(after! lsp-mode
  ;; Disable creation on ts-server .log files
  (setq! lsp-auto-guess-root t
         lsp-signature-doc-lines 1
         lsp-ui-sideline-enable nil
         lsp-lens-enable t
         lsp-elixir-suggest-specs nil
         lsp-elixir-dialyzer-enabled nil
         lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr")))

;; Flycheck
(after! flycheck-credo
  (setq! flycheck-elixir-credo-strict t))

;; Unique buffer name formats
(setq! uniquify-buffer-name-style 'forward)

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
;; TODO: Correctly port this to doom
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
