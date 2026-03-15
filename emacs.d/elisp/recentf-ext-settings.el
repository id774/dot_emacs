;;; recentf-ext-settings.el --- Configure extended recentf behavior -*- lexical-binding: t; -*-

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

(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "")
       ,@body)))

;; recentf
(require 'recentf)

(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 9999)  ;; Number of files to save in recentf
(setq recentf-exclude '(".recentf")) ;; Do not include .recentf itself
(setq recentf-auto-cleanup 'never)   ;; Disable automatic cleanup

;; Save .recentf every 3600 seconds
(run-with-idle-timer
 3600 t
 '(lambda ()
    (with-suppressed-message
      (recentf-save-list))))

(require 'recentf-ext)

;;; recentf-ext-settings.el ends here
