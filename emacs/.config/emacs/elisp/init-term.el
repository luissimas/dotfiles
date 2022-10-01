;;; init-term.el --- Terminal configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Terminal modes configuration.

;;; Code:

(use-package vterm
  :config
  (general-define-key
   :states 'emacs
   :keymaps 'vterm-mode-map
   "C-c"      #'vterm--self-insert
   "C-d"      #'vterm--self-insert
   "C-SPC"    #'vterm--self-insert)
  (defun pada/project-vterm ()
    "Create a vterm buffer in the current project's root directory.
  If a vterm buffer already exists for the current project,
  switch to it. Otherwise, create a new vterm buffer.
  With \\[universal-argument] prefix arg, create a new vterm buffer even if one
  already exists"
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (project-vterm-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer project-vterm-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer vterm-buffer)
        (vterm (generate-new-buffer-name project-vterm-name))))))

(provide 'init-term)
;;; Code:
;;; init-term.el ends here
