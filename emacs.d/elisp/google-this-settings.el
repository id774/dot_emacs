;;; google-this-settings.el --- Short description -*- lexical-binding: t; -*-

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

;; google-this
(require 'google-this)

;; (setq google-this-location-suffix "co.jp")

;; Custom search URL
(defun google-this-url ()
  "URL for google searches."
  (concat google-this-base-url google-this-location-suffix
          "/search?q=%s&hl=ja&num=10&as_qdr=y5&lr=lang_ja"))

;; key binding
(global-set-key (kbd "M-g M-t") 'google-this)

;;; google-this-settings.el ends here
