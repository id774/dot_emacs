;;; auto-async-settings.el --- Configure asynchronous byte compilation for Emacs Lisp -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; Enable asynchronous byte compilation for Emacs Lisp buffers
(when (load-p "auto-async-byte-compile")
  ;; Skip files located under /junk/, and skip the DOT_EMACS bootstrap,
  ;; orchestration and configuration/hook-registration files that
  ;; install_dotemacs.sh also excludes from byte compilation for the same
  ;; reason: they are meant to be loaded from source, and some depend on
  ;; the defun-add-hook macro at compile time.  See doc/GUIDELINES for the
  ;; byte compilation scope policy.
  (setq auto-async-byte-compile-exclude-files-regexp
        "/junk/\\|/\\(init\\|autoloads\\|configs\\|lang-mode\\|screen\\|diminish-settings\\|anything-settings\\)\\.el\\'")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; auto-async-settings.el ends here
