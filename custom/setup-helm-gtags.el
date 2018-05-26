(require 'helm-gtags)

;; Enable helm-gtags-mode in languages that GNU Global supports
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'python-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; customize behaviour
(custom-set-variables
 '(helm-gtags-auto-update t))

(global-set-key (kbd "M-.") 'helm-gtags-find-tag-from-here)
(global-set-key (kbd "M-,") 'helm-gtags-pop-stack)
(global-set-key (kbd "C-c C-f") 'helm-gtags-find-files)
(global-set-key (kbd "C-c g f") 'helm-gtags-parse-file)

(provide 'setup-helm-gtags)
