;;; physical-line.el --- Short description -*- lexical-binding: t; -*-

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

;; Physical line movement
(global-set-key "\C-p" 'previous-window-line)
(global-set-key "\C-n" 'next-window-line)

(defun previous-window-line (n)
  (interactive "p")
  (let ((cur-col
         (- (current-column)
            (save-excursion
              (vertical-motion 0)
              (current-column)))))
    (vertical-motion (- n))
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))

(defun next-window-line (n)
  (interactive "p")
  (let ((cur-col
         (- (current-column)
            (save-excursion
              (vertical-motion 0)
              (current-column)))))
    (vertical-motion n)
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))

;;; physical-line.el ends here
