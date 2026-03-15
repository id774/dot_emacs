;; TRAMP configuration
(when (load-p "tramp")
  ;; Shell prompt pattern used for remote detection
  (setq tramp-shell-prompt-pattern "^.*[#$%>] *")

  ;; Enable TRAMP debug buffer
  (setq tramp-debug-buffer t)

  ;; Default remote access method
  (setq tramp-default-method "scpx")

  ;; Directory for TRAMP auto-save files
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save")

  ;; Verbosity level for TRAMP messages
  (setq tramp-verbose 3)
)
