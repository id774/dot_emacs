;;; ess-compat-bridge.el --- Bridge old/new ESS variable names -*- lexical-binding: t; -*-

;; Run after ESS loads, so we can see which variables actually exist.
(with-eval-after-load 'ess-site
  ;; If the new name exists, set it.
  (when (boundp 'inferior-ess-r-program-name)
    (setq inferior-ess-r-program-name "R"))   ;; 必要ならフルパスに

  ;; If only the old name exists (Debian 9 など), set that instead.
  (when (and (not (boundp 'inferior-ess-r-program-name))
             (boundp 'inferior-R-program-name))
    (setq inferior-R-program-name "R"))

  ;; Optional: alias the new name to the old one on ancient ESS,
  ;; so any later code referring to the new name just works.
  (when (and (boundp 'inferior-R-program-name)
             (not (boundp 'inferior-ess-r-program-name)))
    (defvaralias 'inferior-ess-r-program-name 'inferior-R-program-name)))

(provide 'ess-compat-bridge)
;;; ess-compat-bridge.el ends here

