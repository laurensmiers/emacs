(require 'ggtags)

;; Update on save of file
(setq ggtags-update-on-save t)

;; Enable async update
(setq ggtags-oversize-limit t)

;; Enable ggtags-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'python-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(provide 'setup-gtags)
