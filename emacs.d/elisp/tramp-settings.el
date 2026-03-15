;;; tramp-settings.el --- Short description -*- lexical-binding: t; -*-

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

;;; tramp-settings.el ends here
