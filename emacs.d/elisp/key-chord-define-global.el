;;; key-chord-define-global.el --- Global key-chord shortcuts for common actions -*- lexical-binding: t; -*-

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

;; Toggle auto-complete-mode
(key-chord-define-global "yu" 'auto-complete-mode)

;; Toggle line numbers
(key-chord-define-global "ty" 'linum-mode)

;; Window movement
(key-chord-define-global "io" 'windmove-up)
(key-chord-define-global ",." 'windmove-down)
(key-chord-define-global "hj" 'windmove-left)
(key-chord-define-global "l;" 'windmove-right)

;; Escape
(key-chord-define-global "fg" 'keyboard-escape-quit)

;; Toggle view-mode
(key-chord-define-global "jk" 'toggle-view-mode)

;; Buffer switching
(key-chord-define-global "m," 'previous-buffer)
(key-chord-define-global "ui" 'backward-buffer)

;; Move to buffer beginning/end
(key-chord-define-global "rt" 'beginning-of-buffer)
(key-chord-define-global "vb" 'end-of-buffer)

;; Buffer list
(key-chord-define-global "kl" 'electric-buffer-list)
(key-chord-define-global "nm" 'electric-buffer-list)
(key-chord-define-global "bn" 'buffer-menu)

;; Open file
(key-chord-define-global "df" 'find-file)

;; anything.el
(key-chord-define-global "as" 'anything)
(key-chord-define-global "sd" 'anything-find-files)
(key-chord-define-global ";:" 'anything)

;; Rectangle selection
(key-chord-define-global "we" 'cua-mode)

;; Undo / redo
(key-chord-define-global "zx" 'undo)
(key-chord-define-global "qw" 'redo)

;;; key-chord-define-global.el ends here
