;;; ess-compat-bridge.el --- Bridge old/new ESS variable names -*- lexical-binding: t; -*-

(when (or (> emacs-major-version 24)
          (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
  ;; Emacs 24.4+ only. Do nothing on 24.3- and 23.x.

  ;; Run after ESS loads, so we can see which variables actually exist.
  (with-eval-after-load 'ess-site
    ;; If the new name exists, set it.
    (when (boundp 'inferior-ess-r-program-name)
      (setq inferior-ess-r-program-name "R"))   ;; or full path

    ;; If only the old name exists (Debian 9 etc.), set that instead.
    (when (and (not (boundp 'inferior-ess-r-program-name))
               (boundp 'inferior-R-program-name))
      (setq inferior-R-program-name "R"))

    ;; Optional: alias new -> old on ancient ESS so later code using the new name works.
    (when (and (boundp 'inferior-R-program-name)
               (not (boundp 'inferior-ess-r-program-name)))
      (defvaralias 'inferior-ess-r-program-name 'inferior-R-program-name))))

(provide 'ess-compat-bridge)
;;; ess-compat-bridge.el ends here

