;; Confirm when opening a non-existent file
(add-hook 'find-file-not-found-hooks 'new-file-p)

(defun new-file-p ()
  (interactive)
  (or (y-or-n-p
       (format "\"%s\"not found. Create this file?"
               buffer-file-name))))
