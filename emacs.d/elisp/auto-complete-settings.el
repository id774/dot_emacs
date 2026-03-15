;; Enable auto-complete and configure case sensitivity for Ruby
(when (load-p "auto-complete")
  (global-auto-complete-mode t)

  ;; Use case-sensitive completion in ruby-mode
  (add-hook 'ruby-mode-hook
            (lambda ()
              (make-local-variable 'ac-ignore-case)
              (setq ac-ignore-case nil))))
