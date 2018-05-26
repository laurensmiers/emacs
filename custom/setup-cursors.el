(require 'multiple-cursors)

(global-set-key (kbd "C-x r a") 'mc/edit-lines)
(global-set-key (kbd "C-x r e") 'mc/edit-ends-of-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)

(provide 'setup-cursors)
