;;; cl-compat-bridge.el --- Bridge cl/cl-lib across Emacs 23.4..30.1 -*- lexical-binding: t; -*-

;; Do not try cl-lib on Emacs < 24.3 to avoid recursive load with 3rd-party cl-lib.
(defconst cl-compat--has-cl-lib
  (or (> emacs-major-version 24)
      (and (= emacs-major-version 24) (>= emacs-minor-version 3))))

;; Core load: cl on old Emacs, cl-lib on new Emacs.
(if cl-compat--has-cl-lib
    (require 'cl-lib)
  (require 'cl))

;; Legacy macro/function shims (define only when missing)
(unless (fboundp 'defstruct)
  (defmacro defstruct (&rest args) `(cl-defstruct ,@args)))
(unless (fboundp 'defun*)
  (defmacro defun* (&rest args) `(cl-defun ,@args)))
(unless (fboundp 'defmacro*)
  (defmacro defmacro* (&rest args) `(cl-defmacro ,@args)))

(unless (fboundp 'toggle-read-only)
  (defalias 'toggle-read-only 'read-only-mode))
(unless (fboundp 'which-func-mode)
  (defalias 'which-func-mode 'which-function-mode))

;; Map common CL functions when cl-lib is present
(when (featurep 'cl-lib)
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
  (defmacro loop (&rest body) `(cl-loop ,@body))
  (defmacro pushnew (x place &rest keys) `(cl-pushnew ,x ,place ,@keys))
  (defmacro incf (place &optional delta) `(cl-incf ,place ,(or delta 1)))
  (defmacro decf (place &optional delta) `(cl-decf ,place ,(or delta 1)))
  (defmacro assert (test &optional show-args string &rest args)
    `(cl-assert ,test ,show-args ,string ,@args)))

(provide 'cl-compat-bridge)
;;; cl-compat-bridge.el ends here
