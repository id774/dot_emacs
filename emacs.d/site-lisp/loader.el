;; loader.el
;;
;; After change this file, exec follow command.
;; $ emacs --batch -Q -f batch-byte-compile loader.el

;; auto-install
(when (load-p "auto-install")
  (setq auto-install-directory "~/.emacs.d/site-lisp")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (if global-proxy-use
    (setq url-proxy-services 
      '((concat ("http:" global-proxy-server ":" global-proxy-port))))))

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
