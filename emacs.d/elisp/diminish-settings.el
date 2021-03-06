(when (load-p "diminish")
  (diminish 'isearch-mode)
  ;; (diminish 'gtags-mode "G")
  (diminish 'abbrev-mode "Abbr")
  ;; (diminish 'ac-mode "[tab]")
  ;; (diminish 'font-lock-mode "");
  (defun-add-hook 'lisp-interaction-mode-hook (setq mode-name "Lisp"))
  (defun-add-hook 'emacs-lisp-mode-hook (setq mode-name "elisp"))
  (defun-add-hook 'texinfo-mode-hook (setq mode-name "texi"))
  (defun-add-hook 'change-log-mode-hook (setq mode-name "CL")))
