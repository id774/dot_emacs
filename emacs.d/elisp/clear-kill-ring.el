;;; clear-kill-ring.el --- Utility to clear kill-ring and system clipboard -*- lexical-binding: t; -*-

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

;; clear-kill-ring
(defun clear-kill-ring ()
  "Clear the kill-ring and system clipboard (macOS safe)."
  (interactive)
  (setq kill-ring nil)
  (when (fboundp 'gui-set-selection)
    (gui-set-selection 'CLIPBOARD nil))
  (message "kill-ring and clipboard cleared"))

;;; clear-kill-ring.el ends here
