;;; whitespace-settings.el --- Visualize full-width spaces, tabs, and trailing spaces -*- lexical-binding: t; -*-

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

;; Show full-width spaces, tabs, trailing spaces, and end-of-line markers
;; with the standard whitespace mode, leaving ordinary spaces unmarked.
(when (require 'whitespace nil t)
  (setq whitespace-style
        '(face spaces space-mark tabs tab-mark trailing newline newline-mark))

  ;; Match the full-width space only, so ASCII spaces are not highlighted.
  (setq whitespace-space-regexp "\\(\x3000+\\)")

  ;; Map the characters that were marked before, and no others.
  (setq whitespace-display-mappings
        '((space-mark ?\x3000 [?\x25a1])
          (tab-mark ?\t [?^ ?\t] [?\\ ?\t])
          (newline-mark ?\n [?$ ?\n])))

  (face-spec-set
   'whitespace-space
   '((((class color) (background light))
      (:foreground "blue" :background "unspecified"))
     (t (:foreground "green" :background "unspecified"))))

  (face-spec-set
   'whitespace-tab
   '((((class color) (background light))
      (:foreground "red"
                   :background "unspecified"
                   :strike-through nil
                   :underline t))
     (t (:foreground "purple"
                     :background "unspecified"
                     :strike-through nil
                     :underline t))))

  (face-spec-set
   'whitespace-trailing
   '((((class color) (background light))
      (:foreground "red"
                   :background "unspecified"
                   :strike-through nil
                   :underline t))
     (t (:foreground "purple"
                     :background "unspecified"
                     :strike-through nil
                     :underline t))))

  (when (fboundp 'global-whitespace-mode)
    (global-whitespace-mode 1)))

;;; whitespace-settings.el ends here
