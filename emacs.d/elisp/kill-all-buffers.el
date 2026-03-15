;; Kill all open buffers
(defun kill-all-buffers ()
  (interactive)
  (delete-other-windows)
  (dolist (buf (buffer-list))
    (kill-buffer buf)))
