;;; core-compat-bridge.el --- Small core shims not tied to cl -*- lexical-binding: t; -*-

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
