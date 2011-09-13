;; key-chord.el$B@lMQ%-!<%P%$%s%I@_Dj(B

;; yu $B$G(B auto-complete-mode$B$NM-8z(B/$BL58z$r@Z$jBX$($k(B
(key-chord-define-global "yu" 'auto-complete-mode)

;; $B%&%#%s%I%&0\F0(B
(key-chord-define-global "io" 'windmove-up)
(key-chord-define-global ",." 'windmove-down)
(key-chord-define-global "hj" 'windmove-left)
(key-chord-define-global "l;" 'windmove-right)

;; fg $B$G(B keyboard-escape-quit $B$9$k(B
(key-chord-define-global "fg" 'keyboard-escape-quit)

;; jk $B$G(B view-mode $B$r@Z$jBX$($k(B
(key-chord-define-global "jk" 'toggle-view-mode)

;; $B%P%C%U%!@Z$jBX$((B
(key-chord-define-global "m," 'previous-buffer)
(key-chord-define-global "ui" 'backward-buffer)

;; $B%P%C%U%!@hF,(B/$BKvHx$X$N%+!<%=%k0\F0(B
(key-chord-define-global "rt" 'beginning-of-buffer)
(key-chord-define-global "vb" 'end-of-buffer)

;; $B%9%/%m!<%k(B
(key-chord-define-global "er" 'scroll-down)
(key-chord-define-global "cv" 'scroll-up)

;; $B%P%C%U%!%j%9%H(B
(key-chord-define-global "kl" 'electric-buffer-list)
(key-chord-define-global "nm" 'iswitchb-buffer)
(key-chord-define-global "bn" 'buffer-menu)

;; $B%U%!%$%k$r3+$/(B
(key-chord-define-global "df" 'find-file)

;; Anthing.el
(key-chord-define-global "as" 'anything)
(key-chord-define-global "sd" 'anything-find-files)
(key-chord-define-global ";:" 'anything)

;; $B6k7AA*Br(B
(key-chord-define-global "we" 'cua-mode)

;; $B%"%s%I%%(B/$B%j%I%%(B
(key-chord-define-global "zx" 'undo)
(key-chord-define-global "qw" 'redo)

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
