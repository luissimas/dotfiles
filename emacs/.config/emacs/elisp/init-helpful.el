;;; init-helpful.el --- helpful configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; helpful configuration.

;;; Code:

(use-package helpful
  :config
  (defvar read-symbol-positions-list nil)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(provide 'init-helpful)
;;; init-helpful.el ends here
