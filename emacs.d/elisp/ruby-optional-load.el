;;; ruby-optional-load.el --- Optional Ruby development environment setup -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

(defun ruby-optional-load ()
  ;; ruby-electric: automatic insertion of matching delimiters
  (when (require 'ruby-electric)
    (add-hook 'ruby-mode-hook
              '(lambda () (ruby-electric-mode t))))

  ;; rubydb2x: debugger autoload
  (autoload 'rubydb "rubydb2x"
    "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger."
    t)

  ;; ruby-block: highlight matching block delimiters
  (when (require 'ruby-block)
    (ruby-block-mode t)
    (setq ruby-block-highlight-toggle t)))

;; ruby-mode setup
(when (autoload-p 'ruby-mode "ruby-mode" "Ruby" 'interactive)
  (setq auto-mode-alist
        (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
  (setq interpreter-mode-alist
        (cons '("ruby" . ruby-mode) interpreter-mode-alist))

  ;; inf-ruby: interactive Ruby support
  (autoload 'run-ruby "inf-ruby"
    "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "inf-ruby"
    "Set local key defs for inf-ruby in ruby-mode")

  (add-hook 'ruby-mode-hook
            '(lambda () (inf-ruby-keys)))

  ;; GNU/Linux specific setup
  (cond
   ((eq system-type 'gnu/linux)
    (ruby-optional-load)))

  ;; macOS compatibility for older Emacs
  (cond
   ((eq system-type 'darwin)
    (cond
     ((< emacs-major-version '23)
      (ruby-optional-load))))))

;;; ruby-optional-load.el ends here
