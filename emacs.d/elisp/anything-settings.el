;;; anything-settings.el --- Configure anything sources and key bindings -*- lexical-binding: t; -*-

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

(cond
 ((load-p "anything-config")
  (setq anything-sources
        (list anything-c-source-buffers
              anything-c-source-bookmarks
              anything-c-source-recentf
              anything-c-source-file-name-history
              anything-c-source-locate))

  (define-key global-map "\C-x\ b" 'anything)

  (with-eval-after-load 'anything
    (define-key anything-map (kbd "C-p") 'anything-previous-line)
    (define-key anything-map (kbd "C-n") 'anything-next-line)
    (define-key anything-map (kbd "C-v") 'anything-next-source)
    (define-key anything-map (kbd "M-v") 'anything-previous-source)))

 ;; fallback: minimal buffer switcher
 (t
  (define-key global-map "\C-x\C-b" 'electric-buffer-list)))

;;; anything-settings.el ends here
