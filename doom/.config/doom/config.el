;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Luís Augusto Simas do Nascimento"
      user-mail-address "luissimas@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 15)
      doom-variable-big-font (font-spec :family "JetBrains Mono" :size 16))


(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))
(setq global-prettify-symbols-mode t)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/dox/org/"
      org-hide-emphasis-markers t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Make cloning buffers indirect
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

;; Setting git-gutter signcolumn width
(after! git-gutter-fringe
  (set-fringe-mode '(1 . 0)))

;; Centaur-tabs config
;; (setq centaur-tabs-set-icons t
;;       centaur-tabs-height 15)

;; (map! :n "C-," #'centaur-tabs-backward
;;       :n "C-." #'centaur-tabs-forward
;;       :n "M-." #'centaur-tabs-move-current-tab-to-right
;;       :n "M-," #'centaur-tabs-move-current-tab-to-left)

;; Window mappings
(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right
      :n "M-h" #'evil-window-increase-width
      :n "M-j" #'evil-window-decrease-height
      :n "M-k" #'evil-window-increase-height
      :n "M-l" #'evil-window-decrease-width)

;; Evil mode
(setq +evil-want-o/O-to-continue-comments nil)

(map! :n "L" #'evil-end-of-line
      :n "H" #'evil-beginning-of-line)

;; Projectile projects
;(setq projectile-project-search-path '("~/fun" "~/cati" "~/dox/ufscar" "~/exercism"))

;; Ligatures, I don't actually know how this works
;; (after! elixir-mode-hook
;;   (set-ligatures! 'elixir-mode))

;; Company
(setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-box-scrollbar nil)

;; Dashboard
(setq fancy-splash-image "~/.config/doom/logo.png")

;; Treesitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; Configuring path for elixir-ls
(add-to-list 'exec-path (expand-file-name "~/repos/elixir-ls/"))

;; lsp-ui
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 1
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-width 400
        lsp-ui-doc-max-height 200
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t))
