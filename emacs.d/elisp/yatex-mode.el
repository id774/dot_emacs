;; yatex-mode
(setq auto-mode-alist
  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex2pdf")
(cond
  ((eq system-type 'gnu/linux)
    (setq dvi2-command "evince"))
  ((eq system-type 'darwin)
    (setq dvi2-command "open -a Preview")))
(add-hook 'yatex-mode-hook '(lambda () (setq auto-fill-function nil)))
