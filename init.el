;; hide the welcome screen
(setq inhibit-startup-message t)

;; set garbage collection to higher value
;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold 100000000)

;; important yes-or-no questions can be answered with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; set my theme
(load-theme 'wombat)

;; maximize Emacs at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Save Emacs session
(desktop-save-mode 1)

;; add the custom dir to our load path
(add-to-list 'load-path "~/.emacs.d/custom")

;; add melpa-stable to package-archives
;; IMPORTANT: add (require 'package), else package-archives is not declared (void-variable)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; MUST be called after package-archives is updated
;; Else the automated installation logic is not able to install missing packages
(package-initialize)

;; my required packages
(defconst my-packages
  '(
    undo-tree
    volatile-highlights
    ws-butler
    smartparens
    iedit
    zygospore
    comment-dwim-2
    yasnippet
    sr-speedbar
    company
    irony
    irony-eldoc
    company-irony
    flycheck-irony
    elpy
    py-autopep8
	magit
    helm
    helm-gtags
    helm-swoop
    helm-company
    ))

;; function to install new packages
(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

;; install packages if not yet installed
(install-packages)

;; setup coding
(require 'setup-coding)

;; setup gtags
(require 'setup-gtags)
(require 'setup-editing)

;; setup helm
(require 'setup-helm)
(require 'setup-helm-gtags)

;; setup speedbar
(require 'setup-speedbar)

;; setup autocompletion
(require 'setup-autocompletion)

;; Package zygospore --- revert C-x 1 by pressing C-x 1 again
;; Doesn't work with sr-speedbar
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; Set 'M-g' to 'goto-line', it's faster and what we usually want
(global-set-key (kbd "M-g") 'goto-line)

;; Set 'C-x r i' to 'string-insert-rectangle'
;; Easier than using 'M-x' and searching for it.
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit irony-eldoc elpy irony helm-swoop helm))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
