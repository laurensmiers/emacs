;;; Increase garbage collection threshold during init but leave it to the default value after
;;; There are a LOT of articles/sites/... discussing this:
;;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;; https://jonnay.github.io/emagicians-starter-kit/Emagician-Base.html
;;; ...
(let ((gc-cons-threshold most-positive-fixnum))
  ;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
  (defvar config-file (expand-file-name "config_new.org" user-emacs-directory))
  ;;(defvar project-file (expand-file-name "project.org" user-emacs-directory))

  (when (file-readable-p config-file)
    (org-babel-load-file (expand-file-name config-file)))

  ;; If it exists, load some project-specific configurations.
  ;;(when (file-readable-p project-file)
  ;;  (org-babel-load-file (expand-file-name project-file)))
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(flycheck-clang-tidy org-tree-slide ox-reveal writeroom-mode visual-fill-column org-present clang-format+ dash)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
