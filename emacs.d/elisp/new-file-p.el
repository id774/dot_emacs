;;; new-file-p.el --- Short description -*- lexical-binding: t; -*-

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

;; Confirm when opening a non-existent file
(add-hook 'find-file-not-found-hooks 'new-file-p)

(defun new-file-p ()
  (interactive)
  (or (y-or-n-p
       (format "\"%s\"not found. Create this file?"
               buffer-file-name))))

;;; new-file-p.el ends here
