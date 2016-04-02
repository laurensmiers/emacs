;;; packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; set garbage collection to higher value
;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold 100000000)

;; hide the welcome screen
(setq inhibit-startup-message t)

;; important yes-or-no questions can be answered with y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; my required packages
(defconst my-packages
  '(anzu
    ;; company
    helm
    helm-gtags
    helm-swoop
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    volatile-highlights
    undo-tree
    flycheck
    zygospore))

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

;; set my c-style
(setq c-default-style "linux")

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; set my indentation level
(setq c-basic-offset 2)

;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

;; add the custom dire to our load path
(add-to-list 'load-path "~/.emacs.d/custom")

;; setup helm
(require 'setup-helm)
(require 'setup-helm-gtags)

(require 'setup-cedet)
(require 'setup-editing)

;; use the default window move library keybindings (shift and arrow keys)
(windmove-default-keybindings)

;; flycheck --- on-the-fly syntax check
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; function-args --- completion and showing inline arguments hint for C/C++
(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(tab)] 'moo-complete)
(define-key c++-mode-map  [(tab)] 'moo-complete)

;; show unnecessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent --- guess the indentation offset and use it (for editing foreign files)
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler --- trim spaces from eol
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens --- smart way to handle (), {}, ...
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package zygospore --- revert C-x 1 by pressing C-x 1 again
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set 'M-g' to 'goto-line'
(global-set-key (kbd "M-g") 'goto-line)

;; set my theme
(load-theme 'wombat)

(provide 'init)
;;; init.el ends here
