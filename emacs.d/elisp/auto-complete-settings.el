;;; auto-complete-settings.el --- Configure auto-complete framework and language behavior -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; Enable auto-complete and configure case sensitivity for Ruby
(when (load-p "auto-complete")
  (global-auto-complete-mode t)

  ;; Use case-sensitive completion in ruby-mode
  (add-hook 'ruby-mode-hook
            (lambda ()
              (make-local-variable 'ac-ignore-case)
              (setq ac-ignore-case nil))))

;;; auto-complete-settings.el ends here
