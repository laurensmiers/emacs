;; Package: sr-speedbar
(require 'sr-speedbar)
(add-hook 'emacs-startup-hook (lambda () ; Open sr speedbar on startup
								(sr-speedbar-open)
								))
(setq speedbar-show-unknown-files t) ; Enable speedbar to show all files
(setq speedbar-use-images nil) ; use text for buttons
(setq sr-speedbar-right-side nil) ; put on left side

(provide 'setup-speedbar)
