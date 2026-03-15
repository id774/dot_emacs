;; dired settings

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
