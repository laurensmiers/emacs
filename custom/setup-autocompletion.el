(add-hook 'after-init-hook 'global-company-mode)

;; Add irony as company-backend
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Add irony as flycheck hook
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'irony-eldoc)

;; Add irony-, flycheck-, company-mode to c-mode
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'company-mode)

;; Set tab to autocomplete or indent depending on context
(global-set-key (kbd "<tab>") 'company-indent-or-complete-common)

(provide 'setup-autocompletion)
