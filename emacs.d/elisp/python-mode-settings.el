;; python-pep8
(load-p "python-pep8")

;; Python Path
(setq python-shell-interpreter "/opt/python/current/bin/python")

;; for executable-find
(setq exec-path (cons (expand-file-name "/opt/python/current/bin") exec-path))

;; python-mode
(setq python-mode-hook
  (function (lambda ()
    (local-set-key "\C-c\ p" 'python-pep8)
    (require 'py-autopep8)
    (define-key python-mode-map (kbd "C-c F") 'py-autopep8)
    (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)
)))
