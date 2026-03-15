;;; kill-all-buffers.el --- Command to close all open buffers -*- lexical-binding: t; -*-

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

;; Kill all open buffers
(defun kill-all-buffers ()
  (interactive)
  (delete-other-windows)
  (dolist (buf (buffer-list))
    (kill-buffer buf)))

;;; kill-all-buffers.el ends here
