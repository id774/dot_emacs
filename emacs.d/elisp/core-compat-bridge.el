;;; core-compat-bridge.el --- Small core shims not tied to cl -*- lexical-binding: t; -*-

;;; Code:

;; Version predicate used by other bridges
(defconst core-compat--emacs-24.3+
  (or (> emacs-major-version 24)
      (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
  "True on Emacs 24.3 or newer.")

;; Provide with-eval-after-load for very old Emacs (prior to 24.4)
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Fallback to eval-after-load for old Emacs."
    `(eval-after-load ,file
       '(progn ,@body))))

;; Provide toggle-read-only for old Emacs versions
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

(unless (fboundp 'which-func-mode)
  (defalias 'which-func-mode 'which-function-mode))

(provide 'core-compat-bridge)

;;; core-compat-bridge.el ends here
