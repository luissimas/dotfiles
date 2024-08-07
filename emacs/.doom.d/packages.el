;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; Themes
(package! ef-themes)
(package! modus-themes)
(package! catppuccin :recipe (:host github :repo "catppuccin/emacs"))

;; Async formatting
(package! apheleia)

;; Org-mode enhancements
(package! org-modern)
(package! org-appear)
(package! org-fragtog)

;; Disabling flycheck popup
(package! flycheck-popup-tip :disable t)
(package! flycheck-posframe :disable t)

;; sxhkdrc
(package! sxhkdrc-mode)

;; Support for engrave org-mode code block exports
(package! engrave-faces)

;; Disable jk escape
(package! evil-escape :disable t)

;; Reading enhancements
(package! centered-cursor-mode)
(package! focus)

;; Iex interaction
(package! inf-elixir)

;; Better completion at point
(package! corfu :recipe (:files (:defaults "extensions/*.el") :host github :repo "minad/corfu"))
(package! kind-icon)
(package! cape)

;; Disable nose tests
(package! nose :disable t)

;; Better terminal emulator
(package! eat :recipe (:host codeberg
                       :repo "akib/emacs-eat"
                       :files ("*.el" ("term" "term/*.el") "*.texi"
                               "*.ti" ("terminfo/e" "terminfo/e/*")
                               ("terminfo/65" "terminfo/65/*")
                               ("integration" "integration/*")
                               (:exclude ".dir-locals.el" "*-tests.el"))))

;; Implementing protocols is cool
(package! rfc-mode)

;; Caddy is cool
(package! caddyfile-mode)

;; Zettelkasten management
(package! denote)

;; Screenshot
(package! screenshot :recipe (:repo "tecosaur/screenshot"))

;; A superior elixir mode
(package! elixir-ts-mode)
