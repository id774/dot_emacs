;;; smartchr-settings.el --- Short description -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

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

;;; smartchr-settings.el ends here
