;;; init-ledger.el --- Ledger configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Ledger configuration.

;;; Code:

(use-package ledger-mode
  :mode "\\.journal\\'"
  :hook (ledger-mode . flycheck-mode)
  :hook (ledger-mode . company-mode)
  :hook (ledger-mode . (lambda ()
                         (add-hook 'before-save-hook 'ledger-mode-clean-buffer nil 'make-it-local)))
  :config
  (setq ledger-mode-should-check-version nil
        ledger-report-links-in-register nil
        ledger-clear-whole-transactions t
        ledger-post-amount-alignment-column 60
        ledger-binary-path "ledger"))

(use-package hledger-mode
  :custom
  (hledger-jfile (expand-file-name "~/dox/accounting/accounting.journal"))
  (hledger-reporting-day 1))

(use-package flycheck-ledger
  :after (flycheck ledger-mode))

(provide 'init-ledger)
;;; Code:
;;; init-ledger.el ends here
