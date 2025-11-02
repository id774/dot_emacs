;;; ess-compat-bridge.el --- Bridge old/new ESS variable names -*- lexical-binding: t; -*-

;; Use ESS bridge only on Emacs 24.4 or newer
(if (or (> emacs-major-version 24)
        (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
    (with-eval-after-load 'ess-site
      (cond
       ;; New name exists, expose old name too
       ((and (boundp 'inferior-ess-r-program-name)
             (not (boundp 'inferior-R-program-name)))
        (defvaralias 'inferior-R-program-name 'inferior-ess-r-program-name))
       ;; Old name exists, expose new name too
       ((and (boundp 'inferior-R-program-name)
             (not (boundp 'inferior-ess-r-program-name)))
        (defvaralias 'inferior-ess-r-program-name 'inferior-R-program-name))))
  ;; Emacs 23.x / 24.1-24.3: do nothing
  )

(provide 'ess-compat-bridge)
;;; ess-compat-bridge.el ends here

