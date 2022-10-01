;;; init-org.el --- Org-mode configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Org-mode configuration.

;;; Code:

(defun pada/org-mode-setup ()
  "Set options for `org-mode'. This function is meant to be added to `org-mode-hook'."
  (org-indent-mode)
  (variable-pitch-mode)
  (visual-line-mode)
  (setq line-spacing 1)
  (flyspell-mode)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (org-latex-preview '(16))
  (general-define-key :states 'normal :keymaps 'org-mode-map "<tab>" 'evil-toggle-fold)
  (setq-local electric-pair-inhibit-predicate
              (lambda (c)
                (if (char-equal c ?<) t (electric-pair-default-inhibit c)))))

(use-package org
  :hook
  (org-mode . pada/org-mode-setup)
  :config
  (setq org-hide-emphasis-markers t
        org-pretty-entities t
        org-return-follows-links t
        org-startup-folded t
        org-link-file-path-type 'relative
        org-display-remote-inline-images 'download
        org-startup-with-inline-images t
        org-startup-with-latex-preview nil ; We set the preview in `pada/org-mode-setup', since we can't set the font scale before org starts
        org-cycle-level-faces nil
        org-n-level-faces 4
        org-image-actual-width nil
        org-hidden-keywords '(title)
        org-preview-latex-image-directory (expand-file-name "tmp/ltximg/" user-emacs-directory)
        org-todo-keywords '((sequence "CURRENT(c)" "TODO(t)" "|" "DONE(d)"))
        org-use-fast-todo-selection 'expert
        org-agenda-files '("~/org")
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-tag-alist '(("work" . ?w) ("school" . ?s))
        org-confirm-babel-evaluate nil)

  (add-to-list 'org-modules 'org-tempo)
  (add-to-list 'org-latex-packages-alist '("" "systeme" t))

  ;; Font scaling for different header levels
  (set-face-attribute 'org-level-8 nil :weight 'semi-bold :inherit 'default)
  (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.2)
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.44)
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.728)
  (set-face-attribute 'org-document-title nil :inherit 'org-level-8 :height 2.074)

  ;; Fonts that should always be in fixed-pitch
  (dolist (face '(org-block
                  org-block-begin-line
                  org-code
                  org-document-info-keyword
                  org-meta-line
                  org-table
                  org-verbatim
                  org-checkbox))
    (set-face-attribute `,face nil :inherit 'fixed-pitch))

  (set-face-attribute 'org-block-end-line nil :inherit 'org-block-begin-line)

  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "M-<tab>" 'org-shifttab
   "C-SPC" 'org-toggle-checkbox)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  (defun org-babel-edit-prep:python (babel-info)
    (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
    (lsp))

  (pada/leader-key
    "o" '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Agenda")
    "ot" '(org-todo :which-key "Toggle todo state")
    "oq" '(org-set-tags-command :which-key "Insert tag"))

  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes '("abntex2" "\\documentclass{abntex2}"
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(use-package org-ref)

;; Toggle emphasis markers on cursor
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autokeywords t)
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

;; Toggle latex preview on cursor
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-cycle-headline-bullets nil)
  (org-superstar-headline-bullets-list '("◉" ("🞛" ?◈) "○" "▷"))
  (org-superstar-item-bullet-alist
   '((?* . ?•)
     (?+ . ?➤)
     (?- . ?•)))
  :config
  (set-face-attribute 'org-superstar-item nil :font pada/default-font-family :height 1.2)
  (set-face-attribute 'org-superstar-header-bullet nil :font pada/default-font-family :height 1.2))

;; ;; Animate inline gifs source: https://ivanaf.com/animating_gifs_in_orgmode.html
;; (defun org-inline-image--get-current-image ()
;;   "Return the overlay associated with the image under point."
;;   (car (--select (eq (overlay-get it 'org-image-overlay) t) (overlays-at (point)))))

;; (defun org-inline-image--get (prop)
;;   "Return the value of property PROP for image under point."
;;   (let ((image (org-inline-image--get-current-image)))
;;     (when image
;;       (overlay-get image prop))))

;; (defun org-inline-image-animate ()
;;   "Animate the image if it's possible."
;;   (interactive)
;;   (let ((image-props (org-inline-image--get 'display)))
;;     (when (image-multi-frame-p image-props)
;;       (image-animate image-props))))

;; (defun org-inline-image-animate-auto ()
;;   (interactive)
;;   (when (eq 'org-mode major-mode)
;;     (while-no-input
;;       (run-with-idle-timer 0.3 nil 'org-inline-image-animate))))

;; (setq org-inline-image--get-current-image (byte-compile 'org-inline-image--get-current-image))
;; (setq org-inline-image-animate  (byte-compile 'org-inline-image-animate ))
;; (add-hook 'post-command-hook 'org-inline-image-animate-auto)

;; Presentation
(use-package org-tree-slide)

(defun pada/org-start-presentation ()
  "Start a Org presentation."
  (interactive)
  (org-tree-slide-play-with-timer)
  (flyspell-mode 0)
  (hide-mode-line-mode)
  (text-scale-mode 1))

(defun pada/org-end-presentation ()
  "End a Org presentation."
  (interactive)
  (text-scale-mode 0)
  (flyspell-mode 1)
  (hide-mode-line-mode 0)
  (org-tree-slide-mode 0))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 60)
  (visual-fill-column-center-text t))

(provide 'init-org)
;;; init-org.el ends here
