;; shadow
;; Enable shadow file editing support on GNU/Linux
(cond
 ((eq system-type 'gnu/linux)
  (when (load-p "shadow")
    ;; Automatically unshadow files when opening
    (add-hook 'find-file-hooks 'shadow-on-find-file)

    ;; Enable auto-revert after unshadowing
    (add-hook 'shadow-find-unshadow-hook
              (lambda ()
                (auto-revert-mode 1))))))
