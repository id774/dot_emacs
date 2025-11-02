;;; ess-compat-bridge.el --- Bridge old/new ESS variable names -*- lexical-binding: t; -*-

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

(provide 'ess-compat-bridge)
;;; ess-compat-bridge.el ends here

