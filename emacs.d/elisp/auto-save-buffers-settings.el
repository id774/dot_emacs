;;; auto-save-buffers-settings.el --- Enable enhanced automatic buffer saving -*- lexical-binding: t; -*-

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

;; Enable enhanced automatic buffer saving
(require 'auto-save-buffers-enhanced)

;; Set auto-save interval (seconds)
(setq auto-save-buffers-enhanced-interval 15.0)

;; Enable enhanced automatic buffer saving
(auto-save-buffers-enhanced t)

;; Toggle auto-save activity with C-x x w
(global-set-key "\C-xxw" 'auto-save-buffers-enhanced-toggle-activity)

;;; auto-save-buffers-settings.el ends here
