;;; init-general.el --- general.el configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; general.el configuration.

;;; Code:

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
    "wo" '(other-window :which-key "Other window")

    "x" '(execute-extended-command :which-key "M-x")
    "u" '(universal-argument :which-key "Universal argument")

    "SPC" '(pada/find-file :which-key "Find file in project")

    "f" '(:ignore t :which-key "Find")
    "ff" '(find-file :which-key "Find file")
    "fg" '(consult-ripgrep :which-key "Grep")
    "fc" '((lambda () (interactive) (find-file (expand-file-name "init.el" user-emacs-directory))) :which-key "Find config")
    "fC" '(editorconfig-find-current-editorconfig :which-key "Find project editorconfig")
    "fs" '(save-buffer :which-key "Save file")
    "fS" '(write-file :which-key "Save file as...")

    "s" '(:ignote t :which-key "Search")
    "ss" '(consult-line :which-key "Search line")

    "b" '(:ignore t :which-key "Buffer")
    "bb" '(pada/switch-buffer :which-key "Switch to buffer in project")
    "bB" '(consult-buffer :which-key "Switch to buffer")
    "bk" '(pada/kill-current-buffer :which-key "Kill current buffer")
    "bK" '(kill-buffer :which-key "Kill buffer")
    "bi" '(ibuffer :which-key "Ibuffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
    "bn" '(next-buffer :which-key "Next buffer")
    "bp" '(previous-buffer :which-key "Previous buffer")
    "bN" '(evil-buffer-new :which-key "New buffer")
    "br" '(revert-buffer :which-key "Revert buffer")
    "bc" '(clone-indirect-buffer-other-window :which-key "Clone buffer")

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
    "pv" 'pada/project-vterm
    "p&" nil

    ;; "SPC" '(:keymap perspective-map :which-key "Perspective")

    "t" '(:ignote t :which-key "Toggle")
    "tt" '(pada/load-theme :which-key "Theme")
    "tf" '(flycheck-mode :which-key "Flycheck")
    "tg" '(git-gutter-mode :which-key "Git gutter")
    "tm" '(hide-mode-line-mode :which-key "Modeline")
    "tr" '(rainbow-mode :which-key "Rainbow")
    "tb" '(text-scale-mode :which-key "Big font"))

  ;; Window resizing
  (general-define-key
   "M-h" 'shrink-window-horizontally
   "M-j" 'shrink-window
   "M-k" 'enlarge-window
   "M-l" 'enlarge-window-horizontally))

(provide 'init-general)
;;; init-general.el ends here
