(when (load-p "tramp")
  (setq tramp-shell-prompt-pattern "^.*[#$%>] *")
  (setq tramp-debug-buffer t)
  (setq tramp-default-method "scpx")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save")
  (setq tramp-verbose 3)
)
