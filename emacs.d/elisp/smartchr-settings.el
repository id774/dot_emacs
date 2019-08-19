(cond
  ((< emacs-major-version '25)

;; sequential-command
(when (require 'sequential-command-config)
  (global-set-key "\C-a" 'seq-home)
  (global-set-key "\C-e" 'seq-end)
  (when (require 'org nil t)
    (define-key org-mode-map "\C-a" 'org-seq-home)
    (define-key org-mode-map "\C-e" 'org-seq-end))
  (define-key esc-map "u" 'seq-upcase-backward-word)
  (define-key esc-map "c" 'seq-capitalize-backward-word)
  (define-key esc-map "l" 'seq-downcase-backward-word))

;; smartchr
(when (require 'smartchr)
  (global-set-key (kbd "{") (smartchr '("{" "{)" "{ `!!' }")))
  (global-set-key (kbd ">") (smartchr '(">" "=>" " => " " => '`!!''" " => \"`!!'\"")))
  (global-set-key (kbd "F") (smartchr '("F" "$" "$_" "$_->" "@$"))))

))
