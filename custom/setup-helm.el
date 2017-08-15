(require 'helm)
(require 'helm-config)

;; replace vanilla commands with helm commands
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
;; In vanilla, this is mapped to show-buffers, but I don't use that so map it to helm-mini as well
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; rebind tab to do persistent action
;; we use helm-execute-persistent-action more than helm-select-action (default for <tab>)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB work in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; remap helm-select-action: lists actions
(define-key helm-map (kbd "C-z")  'helm-select-action)

;; remap calculator
(global-set-key (kbd "C-c C-c") 'helm-calcul-expression)

;; TODO: experiment with mark ring   (breadcrumbs something?)
;; TODO: experiment with helm-regexp (build and test regexes)
;; TODO: remember helm-top (helm interface for top program)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-swoop                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locate the helm-swoop folder to your path
(require 'helm-swoop)

;; replace vanilla I-search with helm-swoop
(global-set-key (kbd "C-s") 'helm-swoop)

;; From helm-swoop to helm-multi-swoop-all
;;(define-key helm-swoop-map (kbd "M-s") 'helm-multi-swoop-all-from-helm-swoop)
(define-key helm-swoop-map (kbd "C-s") 'helm-multi-swoop-all-from-helm-swoop)
;; TODO: find out how to switch from multi-swoop to swoop back again

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Next 2 lines make sure when using helm-swoop only the current window/buffer is affected ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

(provide 'setup-helm)
