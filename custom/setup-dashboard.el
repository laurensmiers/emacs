(require 'dashboard)

(dashboard-setup-startup-hook)

;; Set the startup message
(setq dashboard-banner-logo-title "")

;; Set the banner
(setq dashboard-startup-banner 'logo)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        ))


(provide 'setup-dashboard)
