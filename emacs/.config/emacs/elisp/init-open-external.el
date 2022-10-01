;;; init-open-external.el --- Open files in external programs. -*- lexical-binding: t -*-
;;; Commentary:

;; Open files in external and configurable programs.

;;; Code:
(require 'pada-utils)

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

(defun pada/open-external-advice (fun &rest args)
  "Try to match filename in ARGS against patterns in `open-external-associations'.
If a pattern matches, then open the file using the specified command.  If no
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

(provide 'init-open-external)
;;; init-open-external.el ends here
