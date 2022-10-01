;;; init-persp.el --- persp-mode configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; persp-mode configuration.

;;; Code:

(use-package persp-mode
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-nil-hidden t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil
        persp-auto-resume-time -1)
  :init
  (persp-mode))

(provide 'init-persp)
;;; init-persp.el ends here
