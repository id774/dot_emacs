;;; core-compat-bridge.el --- Small compatibility shims for older Emacs versions -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve supported behavior and maintain backward compatibility.
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
    "Evaluate BODY after FILE is loaded on older Emacs versions."
    `(eval-after-load ,file
       '(progn ,@body))))

;; Emacs 29 removed toggle-read-only, so restore it on top of read-only-mode.
;; Older Emacs versions still define it and keep their own implementation.
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

;; auto-async-byte-compile's own `aabc/status' only treats exit status 1
;; as an error, so other nonzero exit statuses (e.g. 2, 126, 127) are
;; misjudged as normal or warning.  Provide a project-owned replacement
;; that treats any nonzero EXITSTATUS as an error, and bridge it in once
;; auto-async-byte-compile is loaded, without editing the third-party file.
(defun dot-emacs-aabc-status (exitstatus buffer)
  "Return the async byte-compile status for EXITSTATUS and BUFFER.
Any nonzero EXITSTATUS is treated as an error.  When EXITSTATUS is
zero, BUFFER is checked for the \":Warning:\" marker to distinguish a
warning from a normal completion."
  (cond
   ((not (= exitstatus 0))
    'error)
   ((with-current-buffer buffer
      (goto-char (point-min))
      (search-forward ":Warning:" nil t))
    'warning)
   (t
    'normal)))

(with-eval-after-load 'auto-async-byte-compile
  (defalias 'aabc/status 'dot-emacs-aabc-status))

(provide 'core-compat-bridge)

;;; core-compat-bridge.el ends here
