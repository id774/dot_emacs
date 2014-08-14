(cond
  ((eq system-type 'gnu/linux)
    (when (load-p "shadow")
      (add-hook 'find-file-hooks 'shadow-on-find-file)
      (add-hook 'shadow-find-unshadow-hook
        (lambda () (auto-revert-mode 1))))))
