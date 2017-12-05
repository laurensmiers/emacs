;; make angry face to get my attention
(setq prog-modes '(c++-mode python-mode erlang-mode java-mode c-mode emacs-lisp-mode scheme-mode prog-mode))
(make-face 'font-lock-angry-face)
(modify-face 'font-lock-angry-face "Red" "Yellow" nil t nil t nil nil)

;; Add keywords to recognize to angry face
(mapc (lambda (mode)
		(font-lock-add-keywords
		 mode
		 '(("\\<\\(FIXME\\)" 1 'font-lock-angry-face t)))
		)
	  prog-modes)
(mapc (lambda (mode)
		(font-lock-add-keywords
		 mode
		 '(("\\<\\(TODO\\)" 1 'font-lock-angry-face t)))
		)
	  prog-modes)

;; default coding style
(setq c-default-style "linux")

;; sane indentation offset
(setq c-basic-offset 4)

;; Tab-space strategy
;; we use spaces, deal with it
(setq-default indent-tabs-mode nil)

;; Enable subword mode for handling CamelCase format
(global-subword-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Python                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'py-autopep8)
(elpy-enable)

(setq elpy-modules (delq 'elpy-module-flycheck elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              Magit (git)                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c m") 'magit-status)

(provide 'setup-coding)
