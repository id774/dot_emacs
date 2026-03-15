;;; init.el --- Short description -*- lexical-binding: t; -*-

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

;; Set up basic paths and load autoloads.el

;; proxy
(load "~/.emacs.d/elisp/proxy")

;; change working directory
(cd "~/")

;; executable paths
(setq exec-path (append '("/usr/local/bin" "/opt/local/bin") exec-path))
(setenv "PATH"
        (concat "/usr/local/bin:/opt/local/bin:/usr/bin:"
                (getenv "PATH")))

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
            (expand-file-name "~/.emacs.d/elisp/3rd-party/ess/lisp")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/helm")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/ruby-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/scala-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/yatex-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/jade-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/nxhtml")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/rhtml")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/haml")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/rinari")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/emacs-async")))

(setq load-path (append my-load-path default-load-path))

;; custom settings
(setq custom-file (expand-file-name "~/.emacs.d/elisp/custom.el"))

;; user identity
(setq user-full-name "id774")
(setq user-mail-address "idnanashi@gmail.com")

;; main entry point
(load "autoloads")

;;; init.el ends here
