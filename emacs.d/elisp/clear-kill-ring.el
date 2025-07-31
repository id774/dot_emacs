;; Clear kill-ring
(defun clear-kill-ring ()
  "Clear the kill-ring and system clipboard (macOS safe)."
  (interactive)
  (setq kill-ring nil)
  (when (fboundp 'gui-set-selection)
    (gui-set-selection 'CLIPBOARD nil))
  (message "kill-ring and clipboard cleared"))
