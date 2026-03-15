;;; global-set-key.el --- Short description -*- lexical-binding: t; -*-

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

;; Scroll the other window
(global-set-key "\M-V" 'scroll-other-window-down)

;; Make C-x p move to the previous window
(define-key ctl-x-map "p"
  #'(lambda (arg) (interactive "p") (other-window (- arg))))

;; Use newline-and-indent by default
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; No more bobcat, no more keyswap!
(cond ((eq window-system 'x)
       (progn
         (global-set-key [delete] 'delete-char)))
      ((eq window-system 'mac)
       t) ;; ok
      (t
       (keyboard-translate ?\C-h ?\C-?)))

;; Toggle line numbers with C-x t
(cond
 ((>= emacs-major-version '23)
  (define-key global-map "\C-x\ t" 'linum-mode))
 ((< emacs-major-version '23)
  (define-key global-map "\C-x\ t" 'wb-line-number-toggle)))

;; Toggle auto-complete-mode with C-x C-y or C-x y
(define-key global-map "\C-x\C-y" 'auto-complete-mode)
(define-key global-map "\C-x\ y" 'auto-complete-mode)

;; Toggle zencoding-mode with C-x C-z or C-x z
(define-key global-map "\C-x\C-z" 'zencoding-mode)
(define-key global-map "\C-x\ z" 'zencoding-mode)

;; Adjust tab width
(defun my-increase-tab-width ()
  (interactive)
  (setq tab-width (+ tab-width 1)))

(defun my-decrease-tab-width ()
  (interactive)
  (when (< 1 tab-width)
    (setq tab-width (- tab-width 1))))

(define-key global-map "\C-c\C-c\ -" 'my-decrease-tab-width)
(define-key global-map "\C-c\C-c\ +" 'my-increase-tab-width)

;; Split horizontally when only one window is present
(defun split-one-window-p ()
  (if (one-window-p)
      (split-window-horizontally)))

(defun split-one-window ()
  (interactive)
  (split-one-window-p))

;; Window movement
(global-set-key [right] 'windmove-right)
(global-set-key [left] 'windmove-left)
(define-key global-map [up] 'windmove-up)
(define-key global-map [down] 'windmove-down)
(setq windmove-wrap-around t)
(define-key global-map [(C shift n)] 'windmove-down)
(define-key global-map [(C shift p)] 'windmove-up)
(define-key global-map [(C shift b)] 'windmove-left)
(define-key global-map [(C shift f)] 'windmove-right)

;; Window splitting
(define-key global-map "\C-c\C-c\C-k" 'delete-window)
(define-key global-map "\C-c\C-c\ k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\C-y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\ y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\C-j" 'split-one-window)
(define-key global-map "\C-c\C-c\ j" 'split-window-horizontally)

;; Move to the next window clockwise
(define-key global-map "\C-c\C-c\C-w" 'other-window)
(define-key global-map "\C-c\C-c\ w" 'other-window)
(define-key global-map "\C-c\C-c\C-c" 'other-window)
(define-key global-map "\C-c\C-c\ c" 'other-window)

;; navi2ch
(defun switch-to-navi2ch()
  (interactive)
  (split-one-window-p)
  (navi2ch))

(define-key global-map "\C-c\C-c\C-i" 'switch-to-navi2ch)
(define-key global-map "\C-c\C-c\ i" 'switch-to-navi2ch)

;; Resize text with C-x 5 and C-x 6
(define-key global-map "\C-x\ 5" 'text-scale-increase)
(define-key global-map "\C-x\ 6" 'text-scale-decrease)

;; Let C-M-g run keyboard-escape-quit
(global-set-key "\C-\M-g" 'keyboard-escape-quit)

;; Let C-x C-k kill the current buffer
(define-key global-map "\C-x\C-k" 'kill-buffer)

;; Make C-h act as backspace
(global-set-key "\C-h" 'delete-backward-char)

;; Bind C-\ to help-command
(global-set-key "\C-\\" 'help-command)

;; Toggle view-mode with C-x C-j or C-x j
(defun toggle-view-mode ()
  (interactive)
  (cond (view-mode
         (toggle-read-only)
         (setq hl-line-mode nil))
        (t
         (toggle-read-only))))

(define-key global-map "\C-x\C-j" 'toggle-view-mode)
(define-key global-map "\C-x\ j" 'toggle-view-mode)

;; Switch buffers with M-n and M-p
(defun previous-buffer ()
  "Select previous window."
  (interactive)
  (bury-buffer))

(defun backward-buffer ()
  "Select backward window."
  (interactive)
  (switch-to-buffer
   (car (reverse (buffer-list)))))

(global-set-key "\M-n" 'previous-buffer)
(global-set-key "\M-p" 'backward-buffer)

;; Buffer list
(define-key global-map "\C-c\C-c\ h" 'electric-buffer-list)

;; Dynamic abbreviation expansion
(define-key global-map [C-tab] 'dabbrev-expand)
(define-key global-map [C-S-tab] 'dabbrev-completion)

;; Make C-x C-w save the current buffer, and use C-x w for write-file
(define-key global-map "\C-x\C-w" 'save-buffer)
(define-key global-map "\C-x\ w" 'write-file)

;; Save the current buffer with C-M-x C-w as well
(global-set-key "\C-\M-x\C-w" 'save-buffer)

;; Bind C-x C-r to find-library
(global-set-key "\C-x\C-r" 'find-library)

;; Move to beginning/end of buffer
(define-key global-map "\C-c\C-c\C-a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\ a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\C-e" 'end-of-buffer)
(define-key global-map "\C-c\C-c\ e" 'end-of-buffer)

;; describe
(define-key global-map "\C-c\C-c\C-d" 'describe-variable)
(define-key global-map "\C-c\C-c\ d" 'describe-variable)
(define-key global-map "\C-c\C-c\C-f" 'describe-function)
(define-key global-map "\C-c\C-c\ f" 'describe-function)
(define-key global-map "\C-c\C-c\C-b" 'describe-bindings)

;; Search and replace
(define-key global-map "\C-c\C-c\C-q" 'query-replace-regexp)
(define-key global-map "\C-c\C-c\ q" 'query-replace-regexp-eval)
(global-set-key "\C-x\C-q" 'query-replace)
(global-set-key "\C-x\ q" 'replace-string)

;; grep
(define-key global-map "\C-c\C-c\C-g" 'grep-find)
(define-key global-map "\C-c\C-c\ g" 'grep)

;; terminal
(global-set-key "\C-x\C-a" 'multi-term)
(global-set-key "\C-x\ a" 'multi-term)
;; (global-set-key "\C-x\C-a" '(lambda ()(interactive)(ansi-term "/bin/zsh")))
;; (global-set-key "\C-x\ a" '(lambda ()(interactive)(ansi-term "/bin/zsh")))

;; Rectangle selection
(define-key global-map "\C-c\C-c\C-z" 'cua-mode)
(define-key global-map "\C-c\C-c\ z" 'cua-mode)

;; Undo / redo
(define-key global-map "\C-c\C-c\C-u" 'undo)
(define-key global-map "\C-c\C-c\ u" 'undo)
(define-key global-map "\C-c\C-c\C-r" 'redo)
(define-key global-map "\C-c\C-c\ r" 'redo)

;; Clear the kill-ring
(global-set-key (kbd "C-c k") 'clear-kill-ring)
(global-set-key (kbd "C-c C-k") 'clear-kill-ring)

;; Byte compilation
(global-set-key (kbd "C-c C-b") 'byte-compile-file)

;; Revert buffers
(defun revert-current-buffer ()
  (interactive)
  (revert-buffer t t))

(defun revert-all-buffers ()
  (interactive)
  (let ((cbuf (current-buffer)))
    (dolist (buf (buffer-list))
      (if (not (buffer-file-name buf)) ; only the file which visit on path
          nil
        (switch-to-buffer buf)
        (revert-buffer t t)))
    (switch-to-buffer cbuf)))

(define-key global-map "\C-c\C-c\C-p" 'revert-current-buffer)
(define-key global-map "\C-c\C-c\ p" 'revert-all-buffers)

;; Confirm before killing all buffers
(defun confirm-kill-all-buffers ()
  (interactive)
  (if (y-or-n-p "kill all buffers?")
      (kill-all-buffers)))

;; Kill all open buffers
(define-key global-map "\C-c\C-c\ 0" 'confirm-kill-all-buffers)
(define-key global-map "\C-x\ 7" 'confirm-kill-all-buffers)

;; Always confirm on C-x C-c
(defun confirm-save-buffers-kill-emacs ()
  (interactive)
  (if (y-or-n-p "quit emacs? ")
      (save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'confirm-save-buffers-kill-emacs)

;; Toggle jaspace-mode to show tabs, full-width spaces, and trailing spaces
(define-key global-map "\C-c\C-c\C-t" 'jaspace-mode)
(define-key global-map "\C-x\ 9" 'jaspace-mode)

;; Delete trailing whitespace
(define-key global-map "\C-c\C-c\ t" 'delete-trailing-whitespace)

;; Toggle proxy usage
(defun global-proxy-use-toggle () ""
  (interactive)
  (setq global-proxy-use
        (not global-proxy-use))
  (message "%s %s"
           "Use Proxy:"
           (if global-proxy-use
               "on" "off")))

(define-key global-map "\C-c\M-c\ p" 'global-proxy-use-toggle)

;;; global-set-key.el ends here
