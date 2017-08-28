;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

;; Set pipe delay to 0 to reduce latency of irony
(setq w32-pipe-read-delay 0)

;; From "setting up irony mode on Windows" :
;; Make sure the path to clang.dll is in emacs' exec_path and shell PATH.
(setenv "PATH"
        (concat
         "C:\\msys64\\usr\\bin" ";"
         "C:\\msys64\\mingw64\\bin" ";"
         (getenv "PATH")
         )
)
(setq exec-path (append '("c:/msys64/usr/bin" "c:/alt/msys64/mingw64/bin")
                        exec-path))


(provide 'setup-windows)
