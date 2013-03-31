;; loader.el
;;
;; After change this file, exec follow command.
;; $ emacs --batch -Q -f batch-byte-compile loader.el

(if global-proxy-use ()
;; auto-install
  (when (load-p "auto-install")
    (setq auto-install-directory "~/.emacs.d/site-lisp")
    (auto-install-update-emacswiki-package-name t)
    (auto-install-compatibility-setup)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

