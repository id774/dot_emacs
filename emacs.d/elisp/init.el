;;; init.el --- Main entry point for DOT_EMACS initialization -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve supported behavior and maintain backward compatibility.
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

;; Apple Silicon Homebrew path
(when (file-directory-p "/opt/homebrew/bin")
  (add-to-list 'exec-path "/opt/homebrew/bin")
  (setenv "PATH"
          (concat "/opt/homebrew/bin:" (getenv "PATH"))))

;;; my-load-path
(defvar default-load-path load-path
  "*Base of `load-path'.
It is used as a default value of target path to search file or
subdirectory under load-path.")

(defvar my-load-path nil
  "*Additional directories prepended to `load-path'.")

(setq my-load-path
      (list "/usr/local/share/emacs/site-lisp"
            (expand-file-name "~/.emacs.d/site-lisp")
            (expand-file-name "~/.emacs.d/elisp")
            (expand-file-name "~/.emacs.d/elisp/3rd-party")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/ruby-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/scala-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/yatex-mode")
            (expand-file-name "~/.emacs.d/elisp/3rd-party/jade-mode")
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

;; native compilation warnings
;; Emacs 28 and newer compile bundled Emacs Lisp in the background, and the
;; historical third-party files under elisp/3rd-party define some of their
;; functions conditionally, so the compiler reports them as unknown.  Those
;; reports are harmless, but they raise a window on every startup.  Keep the
;; entries in the *Warnings* buffer for review and only stop the pop-up.
;; This has to run before autoloads.el pulls the files in.
(cond
 ((boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))
 ((boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors 'silent)))

;; main entry point
(load "autoloads")

;;; init.el ends here
