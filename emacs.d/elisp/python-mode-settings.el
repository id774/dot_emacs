;; python-pep8
(load-p "python-pep8")

;; Python Path
(setq python-shell-interpreter "/opt/python/current/bin/python")

;; for executable-find
(setq exec-path (cons (expand-file-name "/opt/python/current/bin") exec-path))

;; python-mode
(cond
  ((< emacs-major-version '24)
    (progn
      (setq python-mode-hook
        (function (lambda ()
          (local-set-key "\C-c\ p" 'python-pep8)
          (require 'py-autopep8)
          (define-key python-mode-map (kbd "C-c F") 'py-autopep8)        ; バッファ全体のコード整形
          (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region) ; 選択リジョン内のコード整形
        )))
      (add-hook 'python-mode-hook 'flymake-find-file-hook)
      (when (load "flymake" t)
        (defun flymake-pyflakes-init ()
          (let* ((temp-file (flymake-init-create-temp-buffer-copy
                             'flymake-create-temp-inplace))
                 (local-file (file-relative-name
                              temp-file
                              (file-name-directory buffer-file-name))))
            (list "/usr/local/sbin/pyck"  (list local-file))))
        (add-to-list 'flymake-allowed-file-name-masks
                     '("\\.py\\'" flymake-pyflakes-init)))
      (load-library "flymake-cursor")))
  ((>= emacs-major-version '24)
    (progn
      (setq python-mode-hook
        (function (lambda ()
          (local-set-key "\C-c\ p" 'python-pep8)
          (require 'py-autopep8)
          (define-key python-mode-map (kbd "C-c F") 'py-autopep8)
          (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)
        ))))
  ))
