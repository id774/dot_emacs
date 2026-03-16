;;; zlc-settings.el --- Configure zlc minibuffer completion behavior -*- lexical-binding: t; -*-

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

;; Configure zlc (Zsh-like completion) for minibuffer completion.

(when (require 'zlc)
  ;; Enable zlc completion mode
  (zlc-mode t)

  ;; Do not select completion immediately when only one candidate exists
  (setq zlc-select-completion-immediately nil)

  ;; Configure navigation keys in minibuffer completion
  (let ((map minibuffer-local-map))
    (define-key map (kbd "<down>")  'zlc-select-next-vertical)
    (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
    (define-key map (kbd "<right>") 'zlc-select-next)
    (define-key map (kbd "<left>")  'zlc-select-previous)))

;;; zlc-settings.el ends here
