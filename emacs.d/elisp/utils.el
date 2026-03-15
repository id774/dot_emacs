;; utils.el
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
