;;; python-mode-settings.el --- Configure Python development environment -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; python-pep8
(load-p "python-pep8")

;; Python interpreter
(setq python-shell-interpreter "/opt/python/current/bin/python")

;; Add Python to exec-path
(setq exec-path
      (cons (expand-file-name "/opt/python/current/bin") exec-path))

;; python-mode
(setq python-mode-hook
      (function
       (lambda ()
         (local-set-key "\C-c\ p" 'python-pep8)
         (require 'py-autopep8)
         (define-key python-mode-map (kbd "C-c F") 'py-autopep8)
         (define-key python-mode-map (kbd "C-c f") 'py-autopep8-region))))

;;; python-mode-settings.el ends here
