;; IMPORTANT: add (require 'package), else package-archives is not declared (void-variable)
(require 'package)

(setq package-enable-at-startup nil)

;; add melpa-stable to package-archives
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

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
 '(helm-gtags-auto-update t)
 '(package-selected-packages
   (quote
    (elpy linum-relative avy flycheck-irony irony-eldoc company-irony company-c-headers flycheck magit py-autopep8 multiple-cursors helm-gtags helm-swoop zygospore yasnippet volatile-highlights use-package undo-tree smartparens smart-mode-line monokai-theme iedit helm expand-region dashboard comment-dwim-2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
