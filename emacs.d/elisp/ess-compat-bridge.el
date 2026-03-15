;;; ess-compat-bridge.el --- Short description -*- lexical-binding: t; -*-

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

;; Use the ESS bridge only on Emacs 24.4 or newer
(if (or (> emacs-major-version 24)
        (and (= emacs-major-version 24)
             (>= emacs-minor-version 4)))
    (with-eval-after-load 'ess-site
      (cond
       ;; New name exists; provide the old alias
       ((and (boundp 'inferior-ess-r-program-name)
             (not (boundp 'inferior-R-program-name)))
        (defvaralias 'inferior-R-program-name
                     'inferior-ess-r-program-name))
       ;; Old name exists; provide the new alias
       ((and (boundp 'inferior-R-program-name)
             (not (boundp 'inferior-ess-r-program-name)))
        (defvaralias 'inferior-ess-r-program-name
                     'inferior-R-program-name))))
  ;; Emacs 23.x / 24.1-24.3: no bridge needed
  )

(provide 'ess-compat-bridge)

;;; ess-compat-bridge.el ends here
