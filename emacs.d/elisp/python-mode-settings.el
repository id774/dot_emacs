;;; python-mode-settings.el --- Configure Python development environment -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve supported behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; python-pep8
(load-p "python-pep8")

;; Historical /opt/python/current environment: use it when present, but do
;; not override Emacs / python.el's default interpreter selection otherwise.
(let ((python-dir "/opt/python/current/bin")
      (python-bin "/opt/python/current/bin/python"))
  (when (file-directory-p python-dir)
    (add-to-list 'exec-path python-dir))
  (when (file-executable-p python-bin)
    (setq python-shell-interpreter python-bin)))

;; python-mode
(add-hook 'python-mode-hook
          (lambda ()
            (when (fboundp 'python-pep8)
              (local-set-key "\C-c\ p" 'python-pep8))
            (require 'py-autopep8)
            (define-key python-mode-map (kbd "C-c F") 'py-autopep8)
            (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region)))

;;; python-mode-settings.el ends here
