;;; init-screenshot.el --- Screenshot configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Screenshot configuration. https://github.com/tecosaur/screenshot

;;; Code:

(use-package screenshot
  :straight
  (:host github :repo "tecosaur/screenshot" :branch "master" :build (:not compile)))

(provide 'init-screenshot)
;;; init-screenshot.el ends here
