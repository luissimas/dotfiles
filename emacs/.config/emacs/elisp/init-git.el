;;; init-git.el --- Git configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Git-related packages configuration.

;;; Code:

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setf (alist-get 'unpushed magit-section-initial-visibility-alist) 'show)
  (define-key magit-section-mode-map (kbd "<tab>") 'magit-section-toggle)

  ;; Kill magit diff buffer after commit
  (defun pada/kill-magit-diff-buffer ()
    "Kill the magit-diff-buffer for the current repository, This function is meant to be added on `git-commit-setup-hook'."
    (defun kill-magit-diff-buffer ()
      (kill-buffer (magit-get-mode-buffer 'magit-diff-mode)))
    (add-hook 'with-editor-post-finish-hook 'kill-magit-diff-buffer nil t))

  (add-hook 'git-commit-setup-hook 'pada/kill-magit-diff-buffer))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom
  (git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :after git-gutter
  :custom
  (fringes-outside-margins t)
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package magit-todos
  :config
  (setq magit-todos-branch-list nil)
  :init (magit-todos-mode))

(use-package forge
  :after magit)

(provide 'init-git)
;;; init-git.el ends here
