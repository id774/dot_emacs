;;; persistent-scratch.el --- Preserve the *scratch* buffer across sessions -*- lexical-binding: t; -*-

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

;; Keep the *scratch* buffer from being deleted
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#scratch
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg
        (progn
          (setq arg 0)
          (switch-to-buffer "*scratch*")))
    (cond ((= arg 0)
           (message "*scratch* is cleared up."))
          ((= arg 1)
           (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn
                  (my-make-scratch 0)
                  nil)
              t)))

(add-hook 'after-save-hook
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

;;; persistent-scratch.el ends here
