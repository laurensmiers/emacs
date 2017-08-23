(global-set-key [f9]  'start-kbd-macro)
(global-set-key [f10]  'end-kbd-macro)
(global-set-key [f11]  'call-last-kbd-macro)


;; Package zygospore --- revert C-x 1 by pressing C-x 1 again
;; Doesn't work with sr-speedbar
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Set 'M-g' to 'goto-line', it's faster and what we usually want
(global-set-key (kbd "M-g") 'goto-line)

;; Set 'C-x r i' to 'string-insert-rectangle'
;; Easier than using 'M-x' and searching for it.
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

;; hide the welcome screen
(setq inhibit-startup-message t)

;; set garbage collection to higher value
;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold 100000000)

;; important yes-or-no questions can be answered with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; maximize Emacs at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Save Emacs session
(desktop-save-mode 1)

;; Move one window back command
(defun other-window-backward ()
  "Goto previous window"
  (interactive)
  (other-window -1))
(global-set-key (kbd "\C-x p") 'other-window-backward)

;; set my theme
(load-theme 'wombat)

;; highlight line (hl-line)
(global-hl-line-mode 1)
(set-face-background hl-line-face "dark slate grey")

;; smart mode line
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'powerline)
(sml/setup)

(provide 'setup-general)
