;;; init-path.el --- Path configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Path configuration.

;;; Code:

;; System path
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Making emacs search for binaries in node_modules
(use-package add-node-modules-path
  :hook
  (js-mode . add-node-modules-path)
  (typescript-mode . add-node-modules-path))

(provide 'init-path)
;;; init-path.el ends here
