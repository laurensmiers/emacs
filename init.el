;;; Increase garbage collection threshold during init but leave it to the default value after
;;; There are a LOT of articles/sites/... discussing this:
;;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;;; https://jonnay.github.io/emagicians-starter-kit/Emagician-Base.html
;;; ...
(let ((gc-cons-threshold most-positive-fixnum))
  ;; This is the actual config file. It is omitted if it doesn't exist so emacs won't refuse to launch.
  (defvar config-file (expand-file-name "config.org" user-emacs-directory))
  (defvar project-file (expand-file-name "project.org" user-emacs-directory))

  (when (file-readable-p config-file)
    (org-babel-load-file (expand-file-name config-file)))

  ;; If it exists, load some project-specific configurations.
  (when (file-readable-p project-file)
    (org-babel-load-file (expand-file-name project-file)))
)
