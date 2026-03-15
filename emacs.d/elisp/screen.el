;;; screen.el --- Short description -*- lexical-binding: t; -*-

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

;; Update terminal hard status on xterm/screen
(when (and (not window-system)
           (string-match "^xterm\\|^screen" (getenv "TERM"))
           (load-p "term/xterm"))
  (defun-add-hook 'post-command-hook
    "Update terminal hard status."
    (let ((buf (current-buffer)))
      (unless (eq buf hardstatus-update-last-visited)
        (send-string-to-terminal
         (concat "\e]0;"
                 (encode-coding-string
                  (format-mode-line frame-title-format 0)
                  'utf-8 t)
                 "\a"))
        (setq hardstatus-update-last-visited buf))))
  (setq hardstatus-update-last-visited nil))

;; (when (and (load-p "xterm-frobs")
;;            (load-p "xterm-title")
;;            (not window-system))
;;   (string-match "^xterm\\|^screen" (getenv "TERM"))
;;   (xterm-title-mode 1))

;;; screen.el ends here
