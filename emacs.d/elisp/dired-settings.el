;;; dired-settings.el --- Configure Dired behavior and navigation keys -*- lexical-binding: t; -*-

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

;; Use the other dired window as the default copy/move target
(setq dired-dwim-target t)

;; Allow recursive directory copies
(setq dired-recursive-copies 'always)

;; Limit isearch to filenames in dired buffers
(setq dired-isearch-filenames t)

;; Enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; Avoid creating multiple dired buffers when pressing RET
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a")   'dired-find-file)

;; Go up a directory with BS/DEL
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
(define-key dired-mode-map (kbd "<DEL>")       'dired-up-directory)

;; Move between windows with arrow keys
(define-key dired-mode-map (kbd "<left>")  'windmove-left)
(define-key dired-mode-map (kbd "<right>") 'windmove-right)
(define-key dired-mode-map (kbd "<down>")  'windmove-down)
(define-key dired-mode-map (kbd "<up>")    'windmove-up)

;;; dired-settings.el ends here
