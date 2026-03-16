;;; delete-empty-file.el --- Delete empty files automatically after save -*- lexical-binding: t; -*-

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

;; Delete the file and kill the buffer if it is empty
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))

(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (when (y-or-n-p "Delete file and kill buffer?")
      (delete-file
       (buffer-file-name (current-buffer)))
      (kill-buffer (current-buffer)))))

;;; delete-empty-file.el ends here
