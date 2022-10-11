;;; init-mpdel.el --- Mpdel configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Mpdel configuration.

;;; Code:

(use-package mpdel
  :config
  (defun pada/mpdel-toggle-shuffle ()
    "Toggle mpd shuffle mode."
    (interactive)
    (if libmpdel--random
        (libmpdel-playback-unset-random)
      (libmpdel-playback-set-random)))

  (pada/leader-key
    "m" '(:ignore t :which-key "Mpdel")
    "mp" '(libmpdel-playback-play-pause :which-key "Toggle play")
    "ms" '(pada/mpdel-toggle-shuffle :which-key "Toggle shuffle")
    "mb" '(mpdel-browser-open :which-key "Browse")))

(provide 'init-mpdel)
;;; init-mpdel.el ends here
