;; IMPORTANT: add (require 'package), else package-archives is not declared (void-variable)
(require 'package)

(setq package-enable-at-startup nil)

;; add melpa-stable to package-archives
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)

;; MUST be called after package-archives is updated
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
(when (file-readable-p "~/.emacs.d/config.org")
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; If it exists, load some project-specific configurations.
(when (file-readable-p "~/.emacs.d/project.org")
(org-babel-load-file (expand-file-name "~/.emacs.d/project.org")))

(provide 'init)
