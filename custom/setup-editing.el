(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )

(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; show column numbers
(setq column-number-mode 1)

;; Remove scroll-bar, tool-bar and menu-bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; automatically indent when press RET
(global-set-key (kbd "RET") 'newline-and-indent)

;; Map query-replace-regexp to an easier key
(global-set-key (kbd "C-x r r") 'query-replace-regexp)

;; Map query-replace-regexp to an easier key
(global-set-key (kbd "M-p") 'fill-paragraph)

;; Delete trailing whitespace when saving file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

;; Package: undo-tree -- saner, imo, undo with C-/
(require 'undo-tree)
(global-undo-tree-mode)

;; Package: volatile-highlights --- show changes by "undo/yanks/..."
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;; Package: iedit   --- Replace occurences of symbol and highlight them
(require 'iedit)

;; Package: smartparens --- smart way to handle (), {}, ...
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management smartparens ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-package contains the loop macro
(require 'cl)

(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (underscore . "_")
            (back-quote . "`")))

(define-key smartparens-mode-map (kbd "C-c (") 'wrap-with-parens)
(define-key smartparens-mode-map (kbd "C-c [") 'wrap-with-brackets)
(define-key smartparens-mode-map (kbd "C-c {") 'wrap-with-braces)
(define-key smartparens-mode-map (kbd "C-c '") 'wrap-with-single-quotes)
(define-key smartparens-mode-map (kbd "C-c \"") 'wrap-with-double-quotes)
(define-key smartparens-mode-map (kbd "C-c _") 'wrap-with-underscores)
(define-key smartparens-mode-map (kbd "C-c `") 'wrap-with-back-quotes)

(define-key smartparens-mode-map (kbd "C-c s r") 'sp-rewrap-sexp)

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

;; TODO: in manjaro this selects keyboard-layout or something
;;(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

;; TODO: for some reason this does not work
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-h") 'mark-defun)

(smartparens-global-mode t)


;; Package: comment-dwim-2  --- replacement for built-in comment-dwim, more comment features
(require 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)


;; Package: yasnippet --- code template system
(require 'yasnippet)
(yas-global-mode 1)

(provide 'setup-editing)
