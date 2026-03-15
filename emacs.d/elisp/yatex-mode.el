;; yatex-mode
;; Configure YaTeX as the major mode for LaTeX editing.

(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))

;; Load YaTeX on demand
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; Default LaTeX compilation command
(setq tex-command "/usr/local/sbin/platex2pdf")

;; Viewer command depending on platform
(cond
 ((eq system-type 'gnu/linux)
  (setq dvi2-command "evince"))
 ((eq system-type 'darwin)
  (setq dvi2-command "open -a Preview")))

;; Disable auto-fill in YaTeX buffers
(add-hook 'yatex-mode-hook
          '(lambda ()
             (setq auto-fill-function nil)))
