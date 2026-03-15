;;; tab4.el --- Short description -*- lexical-binding: t; -*-

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

;; Toggle local tab width between 4 and 8 columns.
;; Rebuild `tab-stop-list' to match the selected width.
;; Usage: M-x tab4

(defun tab4 (arg)
  "Toggle `tab-width' between 4 and 8.
With ARG, set `tab-width' to 4 if and only if ARG is positive."
  (interactive "P")
  (let ((width (if (integerp arg)
                   (if (> arg 0) 4 8)
                 (if (eq 8 tab-width) 4 8))))
    ;; Apply buffer-local tab width
    (set (make-local-variable 'tab-width) width)

    ;; Rebuild tab stops for the new width
    (set (make-local-variable 'tab-stop-list) nil)
    (let ((n width)
          (max 1024))
      (while (<= n max)
        (setq tab-stop-list
              (nconc tab-stop-list (list n))
              n (+ n width))))

    (message "TAB=%d" width)))

;;; tab4.el ends here
