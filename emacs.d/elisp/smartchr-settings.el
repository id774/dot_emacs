(cond
 ((< emacs-major-version '25)

  ;; sequential-command
  ;; Provide sequential behavior for C-a / C-e.
  ;; Repeated presses move between indentation and line boundaries.
  (when (require 'sequential-command-config)
    (global-set-key "\C-a" 'seq-home)
    (global-set-key "\C-e" 'seq-end)

    ;; Use sequential home/end behavior in org-mode
    (when (require 'org nil t)
      (define-key org-mode-map "\C-a" 'org-seq-home)
      (define-key org-mode-map "\C-e" 'org-seq-end))

    ;; Case conversion helpers
    (define-key esc-map "u" 'seq-upcase-backward-word)
    (define-key esc-map "c" 'seq-capitalize-backward-word)
    (define-key esc-map "l" 'seq-downcase-backward-word))

  ;; smartchr
  ;; Cycle through predefined expansions for certain characters.
  ;; Useful for quickly inserting code patterns.
  (when (require 'smartchr)
    (global-set-key (kbd "{")
                    (smartchr '("{" "{)" "{ `!!' }")))
    (global-set-key (kbd ">")
                    (smartchr '(">" "=>" " => " " => '`!!''" " => \"`!!'\"")))
    (global-set-key (kbd "F")
                    (smartchr '("F" "$" "$_" "$_->" "@$"))))))
