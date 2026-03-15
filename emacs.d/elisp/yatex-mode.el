;;; yatex-mode.el --- Short description -*- lexical-binding: t; -*-

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

;; Configure YaTeX as the major mode for LaTeX editing.

(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))

;; Load YaTeX on demand
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; Default LaTeX compilation command
(setq tex-command "/usr/local/sbin/platex2pdf")

;; Viewer command depending on platform
(cond
 ((eq system-type 'gnu/linux)
  (setq dvi2-command "evince"))
 ((eq system-type 'darwin)
  (setq dvi2-command "open -a Preview")))

;; Disable auto-fill in YaTeX buffers
(add-hook 'yatex-mode-hook
          '(lambda ()
             (setq auto-fill-function nil)))

;;; yatex-mode.el ends here
