;; init.el
;; ロードパスなどの初期設定をして autoloads.el を呼ぶ

;; proxy
(load "~/.emacs.d/elisp/proxy")

;; chdir
(cd "~/")

;; path 系
(setq exec-path (append '("/usr/local/bin" "/opt/local/bin") exec-path))
(setenv "PATH" (concat '"/usr/local/bin:/opt/local/bin:/usr/bin:" (getenv "PATH")))

;;; my-load-path
(defvar default-load-path load-path
  "*Base of `load-path'.
It is used as a default value of target path to search file or
subdirectory under load-path.")
(setq my-load-path
  (list "/usr/local/share/emacs/site-lisp"
  (expand-file-name "~/.emacs.d/site-lisp")
  (expand-file-name "~/.emacs.d/elisp")
  (expand-file-name "~/.emacs.d/elisp/3rd-party")
  (expand-file-name "~/.emacs.d/elisp/3rd-party/helm")
  (expand-file-name "~/.emacs.d/elisp/3rd-party/ruby-mode")
  (expand-file-name "~/.emacs.d/elisp/3rd-party/scala-mode")
  (expand-file-name "~/.emacs.d/elisp/3rd-party/yatex-mode")
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

