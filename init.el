
;; add the custom dir to our load path
(add-to-list 'load-path "~/.emacs.d/custom")

;; add melpa-stable to package-archives
;; IMPORTANT: add (require 'package), else package-archives is not declared (void-variable)
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; MUST be called after package-archives is updated
;; Else the automated installation logic is not able to install missing packages
(package-initialize)

;; my required packages
(defconst my-packages
  '(
    undo-tree
    volatile-highlights
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
    org
    smart-mode-line
    smart-mode-line-powerline-theme
    helm
    helm-gtags
    helm-swoop
    helm-company
    ggtags
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

;; setup general
(require 'setup-general)

;; setup general editing settings
(require 'setup-editing)

;; setup org
(require 'setup-org)

;; setup coding
(require 'setup-coding)

;; setup gtags
(require 'setup-gtags)

;; setup helm
(require 'setup-helm)
(require 'setup-helm-gtags)

;; setup speedbar
(require 'setup-speedbar)

;; setup autocompletion
(require 'setup-autocompletion)

;; setup Windows if our bootloader is Windows
(if (eq system-type 'windows-nt)
	(require 'setup-windows)
)

;; setup gdb
(require 'setup-gdb)

;; start emacs server
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#757575" "#CD5542" "#4A8F30" "#7D7C21" "#4170B3" "#9B55C3" "#68A5E9" "gray43"])
 '(custom-safe-themes
   (quote
	("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" default)))
 '(fci-rule-color "#2e2e2e")
 '(global-company-mode t)
 '(package-selected-packages
   (quote
	(ample-zen-theme ample-theme magit irony-eldoc elpy irony helm-swoop helm)))
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
	((20 . "#dd5542")
	 (40 . "#CC5542")
	 (60 . "#fb8512")
	 (80 . "#baba36")
	 (100 . "#bdbc61")
	 (120 . "#7d7c61")
	 (140 . "#6abd50")
	 (160 . "#6aaf50")
	 (180 . "#6aa350")
	 (200 . "#6a9550")
	 (220 . "#6a8550")
	 (240 . "#6a7550")
	 (260 . "#9b55c3")
	 (280 . "#6CA0A3")
	 (300 . "#528fd1")
	 (320 . "#5180b3")
	 (340 . "#6380b3")
	 (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
