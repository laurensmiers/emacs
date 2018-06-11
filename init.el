;; IMPORTANT: add (require 'package), else package-archives is not declared (void-variable)
(require 'package)

(setq package-enable-at-startup nil)

;; add melpa-stable to package-archives
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; MUST be called after package-archives is updated
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("5f27195e3f4b85ac50c1e2fac080f0dd6535440891c54fcfa62cdcefedf56b1b" default)))
 '(package-selected-packages
   (quote
    (zygospore yasnippet-snippets volatile-highlights use-package undo-tree sr-speedbar smartparens smart-mode-line-powerline-theme py-autopep8 multiple-cursors magit irony-eldoc iedit helm-swoop helm-gtags helm-company flycheck-irony expand-region elpy dashboard company-irony comment-dwim-2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
