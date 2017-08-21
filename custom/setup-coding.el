;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set indentation style depending on number of spaces/tabs in file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

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
(add-hook 'c-mode-hook 'infer-indentation-style)


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
;TODO: define prefix key C-c m for Magit
(global-set-key (kbd "C-c m s") 'magit-status)



(provide 'setup-coding)
