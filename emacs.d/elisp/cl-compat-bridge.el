;;; cl-compat-bridge.el --- Compatibility bridge between cl and cl-lib APIs -*- lexical-binding: t; -*-

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

;; Reuse the core predicate when available, otherwise compute it locally.
(defconst cl-compat--has-cl-lib
  (if (boundp 'core-compat--emacs-24.3+)
      core-compat--emacs-24.3+
    (or (> emacs-major-version 24)
        (and (= emacs-major-version 24)
             (>= emacs-minor-version 3)))))

;; Load cl on older Emacs and cl-lib on newer Emacs.
(if cl-compat--has-cl-lib
    (require 'cl-lib)
  (require 'cl)) ;; Old Common Lisp extensions.

;; Define legacy macro and function shims only when missing.
(unless (fboundp 'defstruct)
  (defmacro defstruct (&rest args) `(cl-defstruct ,@args)))
(unless (fboundp 'defun*)
  (defmacro defun* (&rest args) `(cl-defun ,@args)))
(unless (fboundp 'defmacro*)
  (defmacro defmacro* (&rest args) `(cl-defmacro ,@args)))

;; Map common CL names to cl-lib equivalents when cl-lib is available.
(when (featurep 'cl-lib)
  ;; Functions
  (defalias 'remove-if     #'cl-remove-if)
  (defalias 'remove-if-not #'cl-remove-if-not)
  (defalias 'find-if       #'cl-find-if)
  (defalias 'find-if-not   #'cl-find-if-not)
  (defalias 'position      #'cl-position)
  (defalias 'count         #'cl-count)
  (defalias 'count-if      #'cl-count-if)
  (defalias 'every         #'cl-every)
  (defalias 'some          #'cl-some)
  (defalias 'subsetp       #'cl-subsetp)
  (defalias 'reduce        #'cl-reduce)
  (defalias 'mapcar*       #'cl-mapcar)
  (defalias 'adjoin        #'cl-adjoin)
  (defalias 'pairlis       #'cl-pairlis)
  (defalias 'assoc*        #'cl-assoc)
  (defalias 'rassoc*       #'cl-rassoc)
  ;; Emacs 26.1 and newer provide gensym, so leave the built-in in place
  (unless (fboundp 'gensym)
    (defalias 'gensym #'cl-gensym))
  (unless (fboundp 'first)
    (defalias 'first #'cl-first))
  (unless (fboundp 'rest)
    (defalias 'rest #'cl-rest))
  (unless (fboundp 'second)
    (defalias 'second #'cl-second))
  (unless (fboundp 'third)
    (defalias 'third #'cl-third))
  (unless (fboundp 'fourth)
    (defalias 'fourth #'cl-fourth))

  ;; Macros
  (defmacro loop (&rest body) `(cl-loop ,@body))
  (defmacro pushnew (x place &rest keys) `(cl-pushnew ,x ,place ,@keys))
  (defmacro incf (place &optional delta) `(cl-incf ,place ,(or delta 1)))
  (defmacro decf (place &optional delta) `(cl-decf ,place ,(or delta 1)))
  (defmacro assert (test &optional show-args string &rest args)
    `(cl-assert ,test ,show-args ,string ,@args))

  ;; Common in legacy anything.el and related code.
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

  ;; Keep a minimal lexical-let shim for very old code.
  (unless (fboundp 'lexical-let)
    (defmacro lexical-let (bindings &rest body)
      "Very small shim; not fully equivalent to lexical binding."
      `(let ,bindings ,@body)))
  (unless (fboundp 'lexical-let*)
    (defmacro lexical-let* (bindings &rest body)
      "Very small shim; not fully equivalent to lexical binding."
      `(let* ,bindings ,@body))))

;; Remaining legacy names, which cl-lib provides only with a cl- prefix.
;; The obsolete cl package used to define them as aliases, and bundled
;; third-party code such as anything.el still uses the historical
;; spelling, so keep the names available without loading cl.  A name is
;; bridged only when it is still undefined, so built-in definitions and
;; anything loaded earlier always win.
(when (featurep 'cl-lib)
  (dolist (entry '((get* . cl-get)
                   (random* . cl-random)
                   (rem* . cl-rem)
                   (mod* . cl-mod)
                   (round* . cl-round)
                   (truncate* . cl-truncate)
                   (ceiling* . cl-ceiling)
                   (floor* . cl-floor)
                   (member* . cl-member)
                   (delete* . cl-delete)
                   (remove* . cl-remove)
                   (sort* . cl-sort)
                   (defsubst* . cl-defsubst)
                   (function* . cl-function)
                   ;; Blocks and non-local exits.
                   block return return-from
                   ;; Control structures.
                   do do* do-symbols do-all-symbols
                   psetq psetf progv the locally load-time-value eval-when
                   macrolet symbol-macrolet
                   multiple-value-bind multiple-value-setq multiple-value-call
                   multiple-value-apply multiple-value-list
                   values values-list nth-value
                   ;; Places.
                   letf letf* rotatef shiftf remf callf callf2
                   ;; Types and declarations.
                   typep deftype check-type
                   declaim proclaim
                   define-compiler-macro compiler-macroexpand
                   ;; Lists and conses.
                   list* copy-list ldiff endp tailp list-length
                   acons subst subst-if subst-if-not
                   sublis nsublis nsubst nsubst-if nsubst-if-not
                   caaar caadr cadar caddr cdaar cdadr cddar cdddr
                   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                   fifth sixth seventh eighth ninth tenth
                   ;; Sequences.
                   subseq concatenate copy-seq svref replace fill
                   map mapcan mapcon mapl maplist
                   merge stable-sort search mismatch
                   find position-if position-if-not count-if-not
                   member-if member-if-not
                   assoc-if assoc-if-not rassoc-if rassoc-if-not
                   substitute substitute-if substitute-if-not
                   nsubstitute nsubstitute-if nsubstitute-if-not
                   delete-if delete-if-not
                   remove-duplicates delete-duplicates
                   union nunion intersection nintersection
                   set-difference nset-difference
                   set-exclusive-or nset-exclusive-or
                   notany notevery tree-equal equalp coerce
                   ;; Symbols, numbers and misc.
                   gentemp getf remprop
                   evenp oddp plusp minusp signum isqrt lcm gcd
                   random-state-p make-random-state
                   nreconc revappend))
    (let* ((old (if (consp entry) (car entry) entry))
           (new (if (consp entry)
                    (cdr entry)
                  (intern (concat "cl-" (symbol-name entry))))))
      (when (and (fboundp new) (not (fboundp old)))
        (defalias old new)))))

;; `return' throws to the innermost nil block, so it needs one to exist.
;; The obsolete cl package gave dolist and dotimes an implicit nil block,
;; and bundled third-party code such as anything.el relies on it, so
;; reproduce that behavior when cl itself is not loaded.
(when (and (featurep 'cl-lib)
           (not (featurep 'cl))
           (fboundp 'advice-add)
           (fboundp 'advice-member-p))
  (defun cl-compat-wrap-in-nil-block (fun &rest args)
    "Expand the macro FUN with ARGS inside an implicit nil block."
    `(cl-block nil ,(apply fun args)))
  (dolist (macro '(dolist dotimes))
    (unless (advice-member-p 'cl-compat-wrap-in-nil-block macro)
      (advice-add macro :around 'cl-compat-wrap-in-nil-block))))

;; Some bundled third-party files still load the obsolete cl package.  Doing
;; so replaces the names bridged above with obsolete aliases, and the
;; compiler then reports every historical call in every file expanded later
;; in the same compilation, including files that never mention cl at all.
;; Absorb that here rather than rewriting the bundled code.
;;
;; Only the obsolescence mark is cleared, and only on a name this bridge is
;; responsible for, which is a name the cl to cl-lib rename moved to the same
;; name under the cl- prefix.  The definitions cl installs are left alone, so
;; whatever it alone provides, such as defsetf, keeps working, and a
;; deprecation that is not one of those renames, such as cl-map-extents,
;; keeps warning.
(defun cl-compat--renamed-to (name)
  "Return the cl-lib name NAME was renamed to by the cl to cl-lib rename.
The rename dropped a trailing asterisk, so both loop and sort* are covered."
  (let ((old (symbol-name name)))
    (intern (concat "cl-" (if (string-match "\\`\\(.+\\)\\*\\'" old)
                              (match-string 1 old)
                            old)))))

;; with-eval-after-load runs the form at once when cl is already loaded, so
;; the order in which a file reaches cl and this bridge does not matter.
;; Emacs before 27.1 marks none of these names, where this is a no-op.
(when (and (featurep 'cl-lib) (fboundp 'with-eval-after-load))
  (with-eval-after-load 'cl
    (mapatoms
     (lambda (name)
       (let ((info (get name 'byte-obsolete-info)))
         (when (and (consp info)
                    (symbolp (car info))
                    (eq (car info) (cl-compat--renamed-to name)))
           (put name 'byte-obsolete-info nil)))))))

(provide 'cl-compat-bridge)

;;; cl-compat-bridge.el ends here
