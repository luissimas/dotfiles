;;; init-project.el --- Project.el configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Project.el configuration.
;; Some enhancements to the built-in functionality were take from
;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/

;;; Code:

(defcustom pada/project-root-markers
  '("mix.exs")
  "Files or directories that indicate the root of a project."
  :group 'project
  :type '(repeat string))

(defun pada/project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker pada/project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun pada/project-find-root (path)
  "Search up the PATH for `pada/project-root-markers'."
  (when-let ((root (locate-dominating-file path #'pada/project-root-p)))
    (cons 'transient (expand-file-name root))))

(use-package project
  :config
  ;; (add-to-list 'project-find-functions #'pada/project-find-root)
  (setq project-switch-commands 'project-find-file))

(provide 'init-project)
;;; Code:
;;; init-project.el ends here
