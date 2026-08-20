;;; minibuffer-completion-settings.el --- Configure built-in minibuffer completion -*- lexical-binding: t; -*-

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

;; Configure built-in visible minibuffer completion on GNU Emacs 30+.
(when (boundp 'minibuffer-visible-completions)
  (setq minibuffer-visible-completions t)

  ;; Preserve the familiar zlc navigation keys while using the built-in
  ;; completion implementation.  Reuse the existing conditional arrow-key
  ;; bindings so these keys navigate candidates only while completions are
  ;; visible.
  (when (boundp 'minibuffer-visible-completions-map)
    (define-key minibuffer-visible-completions-map (kbd "C-f")
      (lookup-key minibuffer-visible-completions-map (kbd "<right>")))
    (define-key minibuffer-visible-completions-map (kbd "C-b")
      (lookup-key minibuffer-visible-completions-map (kbd "<left>")))
    (define-key minibuffer-visible-completions-map (kbd "C-n")
      (lookup-key minibuffer-visible-completions-map (kbd "<down>")))
    (define-key minibuffer-visible-completions-map (kbd "C-p")
      (lookup-key minibuffer-visible-completions-map (kbd "<up>")))))

;;; minibuffer-completion-settings.el ends here
