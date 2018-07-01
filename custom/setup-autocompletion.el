;; Add irony as flycheck hook
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; Set tab to autocomplete or indent depending on context
(global-set-key (kbd "<tab>") 'company-indent-or-complete-common)

(provide 'setup-autocompletion)
