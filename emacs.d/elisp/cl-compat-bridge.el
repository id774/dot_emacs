;;; cl-compat-bridge.el --- Bridge cl/cl-lib across Emacs 23.4..30.1 -*- lexical-binding: t; -*-

;;; Code:

;; Use predicate from core if available; otherwise compute locally.
(defconst cl-compat--has-cl-lib
  (if (boundp 'core-compat--emacs-24.3+)
      core-compat--emacs-24.3+
    (or (> emacs-major-version 24)
        (and (= emacs-major-version 24) (>= emacs-minor-version 3)))))

;; Core load: cl on old Emacs, cl-lib on new Emacs.
(if cl-compat--has-cl-lib
    (require 'cl-lib)
  (require 'cl))  ;; old Common Lisp extensions

;; Legacy macro/function shims (define only when missing)
(unless (fboundp 'defstruct)
  (defmacro defstruct (&rest args) `(cl-defstruct ,@args)))
(unless (fboundp 'defun*)
  (defmacro defun* (&rest args) `(cl-defun ,@args)))
(unless (fboundp 'defmacro*)
  (defmacro defmacro* (&rest args) `(cl-defmacro ,@args)))

;; Map common CL functions when cl-lib is present (unprefixed -> cl- prefixed)
(when (featurep 'cl-lib)
  ;; Functions
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
  (defalias 'gensym          #'cl-gensym)

  ;; Macros
  (defmacro loop (&rest body) `(cl-loop ,@body))
  (defmacro pushnew (x place &rest keys) `(cl-pushnew ,x ,place ,@keys))
  (defmacro incf (place &optional delta) `(cl-incf ,place ,(or delta 1)))
  (defmacro decf (place &optional delta) `(cl-decf ,place ,(or delta 1)))
  (defmacro assert (test &optional show-args string &rest args)
    `(cl-assert ,test ,show-args ,string ,@args))
  ;; Frequently used by legacy anything.el and friends
  (unless (fboundp 'flet)
    (defmacro flet (bindings &rest body) `(cl-flet ,bindings ,@body)))
  (unless (fboundp 'labels)
    (defmacro labels (bindings &rest body) `(cl-labels ,bindings ,@body)))
  (unless (fboundp 'case)
    (defmacro case (keyform &rest clauses) `(cl-case ,keyform ,@clauses)))
  (unless (fboundp 'ecase)
    (defmacro ecase (keyform &rest clauses) `(cl-ecase ,keyform ,@clauses)))
  (unless (fboundp 'typecase)
    (defmacro typecase (keyform &rest clauses) `(cl-typecase ,keyform ,@clauses)))
  (unless (fboundp 'etypecase)
    (defmacro etypecase (keyform &rest clauses) `(cl-etypecase ,keyform ,@clauses)))
  (unless (fboundp 'destructuring-bind)
    (defmacro destructuring-bind (pattern expr &rest body)
      `(cl-destructuring-bind ,pattern ,expr ,@body)))

  ;; Very old code sometimes references lexical-let; keep it minimal.
  (unless (fboundp 'lexical-let)
    (defmacro lexical-let (bindings &rest body)
      "Very small shim; not fully equivalent to lexical binding."
      `(let ,bindings ,@body))))

(provide 'cl-compat-bridge)

;;; cl-compat-bridge.el ends here
