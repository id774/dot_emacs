(when (load-p "auto-complete")
  (global-auto-complete-mode t)
  (add-hook 'ruby-mode-hook
    (lambda ()
      (make-local-variable 'ac-ignore-case)
      (setq ac-ignore-case nil))))
