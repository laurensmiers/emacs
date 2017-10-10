(setq gdb-many-windows 1)

;; Select a register number which is unlikely to get used elsewere
(defconst egdbe-windows-config-register 313465989
  "Internal used")

(defvar egdbe-windows-config nil)

(defun set-egdbe-windows-config ()
  (interactive)
  (setq egdbe-windows-config (window-configuration-to-register egdbe-windows-config-register)))

(defun egdbe-restore-windows-config ()
  (interactive)
  (jump-to-register egdbe-windows-config-register))

(defun egdbe-start-gdb (&optional gdb-args)
  ""
  (interactive)
  (set-egdbe-windows-config)
  (call-interactively 'gdb))

(defun egdbe-quit ()
  "finish."
  (interactive)
  (gud-basic-call "quit")
  (egdbe-restore-windows-config))

(defun egdbe-gud-mode-hook ()
  ""
  (local-unset-key (kbd "q"))
  (local-set-key (kbd "q") 'egdbe-quit))

(add-hook 'gud-mode-hook 'egdbe-gud-mode-hook)

(provide 'setup-gdb)
