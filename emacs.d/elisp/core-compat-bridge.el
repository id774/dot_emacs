;;; core-compat-bridge.el --- Small core shims not tied to cl -*- lexical-binding: t; -*-

;;; Code:

;; Version predicates used by other bridges
(defconst core-compat--emacs-24.3+
  (or (> emacs-major-version 24)
      (and (= emacs-major-version 24) (>= emacs-minor-version 3)))
  "True on Emacs 24.3 or newer.")

;; Non-CL aliases that help old Emacs
(unless (fboundp 'toggle-read-only)
  (defalias 'toggle-read-only 'read-only-mode))

(unless (fboundp 'which-func-mode)
  (defalias 'which-func-mode 'which-function-mode))

(provide 'core-compat-bridge)

;;; core-compat-bridge.el ends here
