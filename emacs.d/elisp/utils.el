;;; utils.el --- Short description -*- lexical-binding: t; -*-

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

;; Helper functions used by .emacs

;; See also:
;; http://www.sodan.org/~knagano/emacs/dotemacs.html

(defun autoload-p (function file &optional docstring interactive type)
  "Set autoload for FUNCTION only if FILE is found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))

(defmacro defun-add-hook (hookname &rest sexplist)
  "Add a lambda wrapping SEXPLIST to HOOKNAME."
  `(add-hook ,hookname
             (function
              (lambda ()
                ,@sexplist))))

(defun load-p (loadlib)
  "Load LOADLIB safely without stopping on failure."
  ;; Try loading with missing-ok.  If it fails, emit a message quietly.
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "failed to load %s" loadlib)))
    load-status))

;;; utils.el ends here
