;; $B%-!<%P%$%s%I@_Dj(B

;; $B$3$C$A$N(Bwidow$B$GJT=8$7$J$,$i$"$C$A$N(B*help*$B$r%9%/%m!<%k$H$+!#(B
(global-set-key "\M-V" 'scroll-other-window-down)

;; C-x p $B$G(B C-x o $B$N5U$NF0:n$r$9$k(B
(define-key ctl-x-map "p"
  #'(lambda (arg) (interactive "p") (other-window (- arg))))

;; $BIaCJ!"%$%s%G%s%H$9$k$h$&$K$9$k(B
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key "\C-j" 'newline)

;; No more bobcat, no more keyswap!
(cond ((eq window-system 'x)
       (progn
	 (global-set-key [delete] 'delete-char)))
      ((eq window-system 'mac)
       t) ;; ok
      (t (keyboard-translate ?\C-h ?\C-?)))

;; C-x t $B$G9T?tI=<((B/$BHsI=<((B
(cond
  ((>= emacs-major-version '23)
    (define-key global-map "\C-x\ t" 'linum-mode)))

;; C-x C-y $B$^$?$O(B C-x y $B$G(B auto-complete-mode $B$NM-8z(B/$BL58z$r@Z$jBX$($k(B
(define-key global-map "\C-x\C-y" 'auto-complete-mode)
(define-key global-map "\C-x\ y" 'auto-complete-mode)

;; $B%&%#%s%I%&$,(B1$B$D$7$+$J$$>l9g$O=D$KJ,3d$9$k4X?t(B
(defun split-one-window-p ()
  (if (one-window-p)
    (split-window-horizontally)))
(defun split-one-window ()
  (interactive)
  (split-one-window-p))

;; $B%&%#%s%I%&0\F0(B
(global-set-key [right] 'windmove-right)
(global-set-key [left] 'windmove-left)
(define-key global-map [up] 'windmove-up)
(define-key global-map [down] 'windmove-down)
(setq windmove-wrap-around t)
(define-key global-map [(C shift n)] 'windmove-down)
(define-key global-map [(C shift p)] 'windmove-up)
(define-key global-map [(C shift b)] 'windmove-left)
(define-key global-map [(C shift f)] 'windmove-right)

;; $B%&%#%s%I%&J,3d(B
(define-key global-map "\C-c\C-c\C-k" 'delete-window)
(define-key global-map "\C-c\C-c\ k" 'delete-other-windows)
(define-key global-map "\C-c\C-c\C-y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\ y" 'split-window-vertically)
(define-key global-map "\C-c\C-c\C-j" 'split-one-window)
(define-key global-map "\C-c\C-c\ j" 'split-window-horizontally)

;; $BJ,3d$7$?%&%#%s%I%&$r;~7W2s$j$K0\F0(B
(define-key global-map "\C-c\C-c\C-w" 'other-window)
(define-key global-map "\C-c\C-c\ w" 'other-window)
(define-key global-map "\C-c\C-c\C-c" 'other-window)
(define-key global-map "\C-c\C-c\ c" 'other-window)

;; Navi2ch
(defun switch-to-navi2ch()
  (interactive)
  (split-one-window-p)
  (navi2ch))
(define-key global-map "\C-c\C-c\C-i" 'switch-to-navi2ch)
(define-key global-map "\C-c\C-c\ i" 'switch-to-navi2ch)

;; $B9TKv$N6uGr$r0l3g:o=|$9$k(B
(define-key global-map "\C-c\C-c\ t" 'delete-trailing-whitespace)

;; $B%?%V(B, $BA43Q%9%Z!<%9!"2~9TD>A0$NH>3Q%9%Z!<%9$rI=<($9$k(B($B%H%0%k$GF0:n(B)
(define-key global-map "\C-c\C-c\C-t" 'jaspace-mode)

;; C-M-g $B$G$b(B keyboard-escape-quit $B$9$k(B
(global-set-key "\C-\M-g" 'keyboard-escape-quit)

;; C-x C-k $B$G$b(B kill buffer$B$9$k(B
(define-key global-map "\C-x\C-k" 'kill-buffer)

;; C-h $B$r(B backspace $B$K$9$k(B
(global-set-key "\C-h" 'delete-backward-char)

;; C-\ $B$r(B help-command $B$K$9$k(B
(global-set-key "\C-\\" 'help-command)

;; C-x C-j $B$^$?$O(B C-x j $B$G(B view-mode $B$r@Z$jBX$($k(B
(defun toggle-view-mode ()
  (interactive)
  (cond (view-mode
      (view-mode nil)
      (setq hl-line-mode nil))
    (t
      (view-mode))))
(define-key global-map "\C-x\C-j" 'toggle-view-mode)
(define-key global-map "\C-x\ j" 'toggle-view-mode)

;; $B%P%C%U%!$r(BM-n,M-p$B$G@Z$jBX$((B
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

;; $B%P%C%U%!%j%9%H(B
(define-key global-map "\C-x\C-b" 'electric-buffer-list)
(define-key global-map "\C-x\ b" 'anything)
(define-key global-map "\C-c\C-c\ b" 'iswitchb-buffer)

;; Anything
(global-set-key (kbd "C-;") 'anything)
(global-set-key (kbd "C-:") 'anything)

;; $BF0E*N,8lJQ49(B
(define-key global-map [C-tab] 'dabbrev-expand)
(define-key global-map [C-S-tab] 'dabbrev-completion)

;; C-x C-w$B$r>e=q$-J]B8$K$9$k(B($BJLL>J]B8$O(BC-x w)
(define-key global-map "\C-x\C-w" 'save-buffer)
(define-key global-map "\C-x\ w" 'write-file)

;; C-M-x C-w$B$G$b>e=q$-J]B8$9$k(B
(global-set-key "\C-\M-x\C-w" 'save-buffer)

;; C-x C-r$B$NB8:_0U5A$,L5$$$N$G%i%$%V%i%j8!:w$K3d$jEv$F(B
(global-set-key "\C-x\C-r" 'find-library)

;; $B%P%C%U%!@hF,(B/$BKvHx$X$N%+!<%=%k0\F0(B
(define-key global-map "\C-c\C-c\C-a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\ a" 'beginning-of-buffer)
(define-key global-map "\C-c\C-c\C-e" 'end-of-buffer)
(define-key global-map "\C-c\C-c\ e" 'end-of-buffer)

;; Describe
(define-key global-map "\C-c\C-c\C-d" 'describe-variable)
(define-key global-map "\C-c\C-c\ d" 'describe-variable)
(define-key global-map "\C-c\C-c\C-f" 'describe-function)
(define-key global-map "\C-c\C-c\ f" 'describe-function)
(define-key global-map "\C-c\C-c\C-b" 'describe-bindings)

;; $B8!:w(B/$BCV49(B
(define-key global-map "\C-c\C-c\C-q" 'query-replace-regexp)
(define-key global-map "\C-c\C-c\ q" 'query-replace-regexp-eval)
(global-set-key "\C-x\C-q" 'query-replace)
(global-set-key "\C-x\ q" 'replace-string)

;; grep
(define-key global-map "\C-c\C-c\C-g" 'grep-find)
(define-key global-map "\C-c\C-c\ g" 'grep)

;; ansi-term
(global-set-key "\C-x\C-a" '(lambda ()(interactive)(ansi-term "/bin/zsh")))
(global-set-key "\C-x\ a" '(lambda ()(interactive)(ansi-term "/bin/zsh")))

;; $B6k7AA*Br(B
(define-key global-map "\C-c\C-c\C-z" 'cua-mode)
(define-key global-map "\C-c\C-c\ z" 'cua-mode)

;; $B%"%s%I%%(B/$B%j%I%%(B
(define-key global-map "\C-c\C-c\C-u" 'undo)
(define-key global-map "\C-c\C-c\ u" 'undo)
(define-key global-map "\C-c\C-c\C-r" 'redo)
(define-key global-map "\C-c\C-c\ r" 'redo)

;; $B%P%C%U%!:FFI$_9~$_(B
(defun revert-current-buffer ()
  (interactive)
  (revert-buffer t t))
(defun revert-all-buffers ()
  (interactive)
  (let ((cbuf (current-buffer)))
    (dolist (buf (buffer-list))
      (if (not (buffer-file-name buf)) ;only the file which visit on path
   nil
 (switch-to-buffer buf)
 (revert-buffer t t)))
    (switch-to-buffer cbuf)
    ))
(define-key global-map "\C-c\C-c\C-p" 'revert-current-buffer)
(define-key global-map "\C-c\C-c\ p" 'revert-all-buffers)

;; $B3+$$$F$$$k$9$Y$F$N%P%C%U%!$r(Bkill$B$9$k(B
(define-key global-map "\C-c\C-c\ 0" 'kill-all-buffers)

;; C-x C-c$B$GI,$:3NG'$9$k(B
(defun confirm-save-buffers-kill-emacs ()
  (interactive)
  (if (yes-or-no-p "quit emacs? ")
    (save-buffers-kill-emacs)))
(global-set-key "\C-x\C-c" 'confirm-save-buffers-kill-emacs)

;; Proxy$B$N%*%s%*%U$r@Z$jBX$($k$9$k4X?t(B
(defun global-proxy-use-toggle () ""
  (interactive)
  (setq global-proxy-use
        (not global-proxy-use))
  (setq twitter1-proxy-use
        (not twitter1-proxy-use))
  (setq twitter2-proxy-use
        (not twitter2-proxy-use))
  (setq twitter3-proxy-use
        (not twitter3-proxy-use))
  (setq twitter4-proxy-use
        (not twitter4-proxy-use))
  (setq twitter5-proxy-use
        (not twitter5-proxy-use))
  (setq twitter6-proxy-use
        (not twitter6-proxy-use))
  (message "%s %s"
           "Use Proxy:"
           (if global-proxy-use
               "on" "off")))
(define-key global-map "\C-c\M-c\ p" 'global-proxy-use-toggle)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
