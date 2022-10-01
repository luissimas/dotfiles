;;; pada-utils.el --- Configuration utilities. -*- lexical-binding: t -*-
;;; Commentary:

;; General utility functions

;;; Code:

(defun pada/is-laptop ()
  "Return if the current session is running on a laptop."
  (file-exists-p "/sys/class/power_supply/BAT1"))

(defun pada/run-shell-command (command)
  "Run COMMAND in the default user shell."
  (message command)
  (start-process-shell-command "Open external process" nil (concat "exec nohup " command " >/dev/null")))

(defun pada/kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun pada/find-file ()
  "Wrapper around `find-file'.
If the current file is in a project, use `project-find-file',
otherwise use the built-in `find-file'."
  (interactive)
  (if (project-current)
      (project-find-file)
    (call-interactively 'find-file)))

(defun pada/switch-buffer ()
  "Wrapper around `consult-buffer'.
If the current buffer is in a project, use `project-switch-to-buffer',
otherwise use `consult-buffer'."
  (interactive)
  (if (project-current)
      (call-interactively 'project-switch-to-buffer)
    (call-interactively 'consult-buffer)))

(defun pada/load-theme (theme)
  "Improvement over the default `load-theme'.
Load THEME and disable all themes that were loaded before."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (load-theme theme t)
  (dolist (theme (cdr custom-enabled-themes))
    (disable-theme theme)))

(provide 'pada-utils)
;;; pada-utils.el ends here
