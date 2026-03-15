;;; jaspace-mode.el --- Highlight full-width spaces, tabs, and trailing spaces -*- lexical-binding: t; -*-

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

;; Show tabs, full-width spaces, and trailing spaces
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes
          (append jaspace-modes
                  (list 'php-mode
                        'yaml-mode
                        'haml-mode
                        'javascript-mode
                        'scala-mode
                        'erlang-mode
                        'scheme-mode
                        'lisp-mode
                        'ruby-mode
                        'python-mode
                        'latex-mode
                        'yatex-mode
                        'markdown-mode
                        'text-mode
                        'fundamental-mode))))

  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))

  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))

  (add-hook 'jaspace-mode-off-hook
            (lambda ()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))

  (add-hook 'jaspace-mode-hook
            (lambda ()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))

                (face-spec-set
                 'jaspace-highlight-jaspace-face
                 '((((class color) (background light))
                    (:foreground "blue"))
                   (t (:foreground "green"))))

                (face-spec-set
                 'jaspace-highlight-tab-face
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
                 'trailing-whitespace
                 '((((class color) (background light))
                    (:foreground "red"
                                 :background "unspecified"
                                 :strike-through nil
                                 :underline t))
                   (t (:foreground "purple"
                                   :background "unspecified"
                                   :strike-through nil
                                   :underline t))))))))

;;; jaspace-mode.el ends here
