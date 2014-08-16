;; python-mode
(cond
  ((eq system-type 'gnu/linux)
    ;; python
    (when (autoload 'python-mode "python-mode" "Python editing mode." t)
      (setq python-mode-hook
        (function (lambda ()
          (local-set-key "\C-c\ p" 'python-pep8))))
      (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
      (setq interpreter-mode-alist (cons '("python" . python-mode)
                                         interpreter-mode-alist)))
    ;; flymake for python
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
    (load-library "flymake-cursor"))
  ((eq system-type 'darwin)
    (setq python-mode-hook
      (function (lambda ()
        (local-set-key "\C-c\ p" 'python-pep8))))))
