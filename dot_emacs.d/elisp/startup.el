;; startup.el
;; ���ɥѥ��ʤɤν������򤷤�autoloads.el��Ƥ�

;; proxy
(load "~/.emacs.d/elisp/proxy")

;; chdir
(cd "~/")

;; path ��
(setq exec-path (append '("/usr/local/bin" "/opt/local/bin") exec-path))
(setenv "PATH" (concat '"/usr/local/bin:/opt/local/bin:/usr/bin:" (getenv "PATH")))

;;; my-load-path
;;; Emacs 23.2 �ʹߤ� CEDET ɸ����ܤΤ�����ɥѥ����̤��ʤ�
(defvar default-load-path load-path
  "*Base of `load-path'.
It is used as a default value of target path to search file or
subdirectory under load-path.")
(cond
  ((> emacs-major-version '22)
    (cond
      ((> emacs-minor-version '1)
        (setq my-load-path
          (list "/usr/local/share/emacs/site-lisp"
          (expand-file-name "~/.emacs.d/elisp")
          (expand-file-name "~/.emacs.d/elisp/3rd-party")
          (expand-file-name "~/.emacs.d/elisp/3rd-party/jde/lisp")
          (expand-file-name "~/.emacs.d/elisp/3rd-party/cedet/common")
          (expand-file-name "~/.emacs.d/elisp/3rd-party/ecb")
          (expand-file-name "~/.emacs.d/elisp/3rd-party/nxhtml")
          (expand-file-name "~/.emacs.d/elisp/3rd-party/rhtml")
          (expand-file-name "~/.emacs.d/elisp/3rd-party/rinari")))))
        (setq my-load-path
          (list "/usr/local/share/emacs/site-lisp"
	        (expand-file-name "~/.emacs.d/elisp")
	        (expand-file-name "~/.emacs.d/elisp/3rd-party")
	        (expand-file-name "~/.emacs.d/elisp/3rd-party/jde/lisp")
	        (expand-file-name "~/.emacs.d/elisp/3rd-party/ecb")
	        (expand-file-name "~/.emacs.d/elisp/3rd-party/nxhtml")
	        (expand-file-name "~/.emacs.d/elisp/3rd-party/rhtml")
	        (expand-file-name "~/.emacs.d/elisp/3rd-party/rinari")))))
      (setq my-load-path
        (list "/usr/local/share/emacs/site-lisp"
        (expand-file-name "~/.emacs.d/elisp")
        (expand-file-name "~/.emacs.d/elisp/3rd-party")
        (expand-file-name "~/.emacs.d/elisp/3rd-party/jde/lisp")
        (expand-file-name "~/.emacs.d/elisp/3rd-party/ecb")
        (expand-file-name "~/.emacs.d/elisp/3rd-party/nxhtml")
        (expand-file-name "~/.emacs.d/elisp/3rd-party/rhtml")
        (expand-file-name "~/.emacs.d/elisp/3rd-party/rinari")))
(setq load-path (append my-load-path default-load-path))

;; custom
(setq custom-file (expand-file-name "~/.emacs.d/elisp/custom.el"))

;; whoami
(setq user-full-name "id774")
(setq user-mail-address "idnanashi@gmail.com")

;; main
(load "autoloads")

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
