;;; cl-compat-bridge.el --- Bridge cl/cl-lib across Emacs 23.4..30.1 -*- lexical-binding: t; -*-

;; Load cl-lib when available; otherwise fall back to cl (for Emacs 23.x).
;; Also provide common legacy aliases so old code using unprefixed CL APIs
;; works on modern Emacs without requiring 'cl' (and without deprecation warnings).

(eval-when-compile (require 'cl-lib nil t))

;; Legacy struct macro
(unless (fboundp 'defstruct)
  (defmacro defstruct (&rest args)
    `(cl-defstruct ,@args)))

;; Legacy defun* macro
(unless (fboundp 'defun*)
  (defmacro defun* (&rest args)
    `(cl-defun ,@args)))

(cond
 ((require 'cl-lib nil t)
  ;; Define legacy function aliases (only if missing)
  (defalias 'remove-if       #'cl-remove-if)
  (defalias 'remove-if-not   #'cl-remove-if-not)
  (defalias 'find-if         #'cl-find-if)
  (defalias 'find-if-not     #'cl-find-if-not)
  (defalias 'position        #'cl-position)
  (defalias 'count           #'cl-count)
  (defalias 'count-if        #'cl-count-if)
  (defalias 'every           #'cl-every)
  (defalias 'some            #'cl-some)
  (defalias 'subsetp         #'cl-subsetp)
  (defalias 'reduce          #'cl-reduce)
  (defalias 'mapcar*         #'cl-mapcar)
  (defalias 'adjoin          #'cl-adjoin)
  (defalias 'pairlis         #'cl-pairlis)
  (defalias 'assoc*          #'cl-assoc)
  (defalias 'rassoc*         #'cl-rassoc)

  ;; Legacy macros
  (defmacro loop (&rest body) `(cl-loop ,@body))
  (defmacro pushnew (x place &rest keys)
    `(cl-pushnew ,x ,place ,@keys))
  (defmacro incf (place &optional delta)
    `(cl-incf ,place ,(or delta 1)))
  (defmacro decf (place &optional delta)
    `(cl-decf ,place ,(or delta 1)))
  (defmacro assert (test-form &optional show-args string &rest args)
    `(cl-assert ,test-form ,show-args ,string ,@args)))

 (t
  ;; Old Emacs: no cl-lib → fall back to cl (no deprecation warning on 23.x)
(require 'cl-compat-bridge)))

(provide 'cl-compat-bridge)
;;; cl-compat-bridge.el ends here
