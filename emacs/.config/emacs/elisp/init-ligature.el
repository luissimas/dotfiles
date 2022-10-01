;;; init-ligature.el --- Font ligature configuration . -*- lexical-binding: t -*-
;;; Commentary:

;; Font ligature configuration.

;;; Code:

(use-package ligature
  :straight '(:host github :repo "mickeynp/ligature.el")
  :config
  ;; Iosevka ligatures
  (ligature-set-ligatures 'prog-mode
                          '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "-->" "--->" "->-" ">-" ">>-"
                            "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "==>" "===>" "=>=" ">=" ">>="
                            "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "<!--" "<!---"
                            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "<>" "===" "!=="
                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+:" "-:" "=:" ":>" "__"
                            "(* *)" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!---" "<***>"))
  (global-ligature-mode))

(provide 'init-ligature)
;;; Code:
;;; init-ligature.el ends here
