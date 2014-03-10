(when (and (not window-system)
     (string-match "^xterm\\|^screen" (getenv "TERM"))
     (load-p "term/xterm"))
  (defun-add-hook 'post-command-hook
    "update terminal hard status."
    (let ((buf (current-buffer)))
      (unless (eq buf hardstatus-update-last-visited)
  (send-string-to-terminal
   (concat "\e]0;"
     (encode-coding-string
      (format-mode-line frame-title-format 0)
      'utf-8 t)
     "\a"))
  (setq hardstatus-update-last-visited buf))))
  (setq hardstatus-update-last-visited nil))
;; (when (and (load-p "xterm-frobs")
;;      (load-p "xterm-title")
;;      (not window-system))
;;      (string-match "^xterm\\|^screen" (getenv "TERM")))
;;   (xterm-title-mode 1))
