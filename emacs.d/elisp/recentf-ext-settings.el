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
