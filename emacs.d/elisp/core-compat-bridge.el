;;; core-compat-bridge.el --- Small compatibility shims for older Emacs versions -*- lexical-binding: t; -*-

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

;; Version predicate shared by other compatibility layers.
(defconst core-compat--emacs-24.3+
  (or (> emacs-major-version 24)
      (and (= emacs-major-version 24)
           (>= emacs-minor-version 3)))
  "True on Emacs 24.3 or newer.")

;; Provide with-eval-after-load on very old Emacs versions.
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Fallback to eval-after-load on old Emacs."
    `(eval-after-load ,file
       (lambda () ,@body))))

;; Provide toggle-read-only on old Emacs versions.
(unless (fboundp 'toggle-read-only)
  (defun toggle-read-only (&optional arg)
    "Toggle read-only status of the current buffer.
With prefix ARG, enable if ARG > 0, otherwise disable."
    (interactive "P")
    (let* ((n (and arg (prefix-numeric-value arg)))
           (mode-arg (if (null arg)
                         'toggle
                       (if (> n 0) 1 -1))))
      (read-only-mode mode-arg))))

;; Alias which-func-mode on older Emacs.
(unless (fboundp 'which-func-mode)
  (defalias 'which-func-mode 'which-function-mode))

(provide 'core-compat-bridge)

;;; core-compat-bridge.el ends here
