(require 'org)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; when ending TODO (C-C C-t) end with a note + timestamp
(setq org-log-done 'note)
;; Specify root dir to search for agenda files, TODOs, ...
(setq org-agenda-files '("~/org"))
;; Add extra states for keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-export-with-sub-superscripts nil)

;; Preserve indentation in SRC blocks
(setq org-src-preserve-indentation t)

;; Specify which languages are allowed to run inside org-mode
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (ditaa . t))
 )

;; Tell org where to look for ditaa
(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0_10.jar")

(provide 'setup-org)
