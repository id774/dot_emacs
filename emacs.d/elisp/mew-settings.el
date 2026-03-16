;;; mew-settings.el --- Configure Mew email client integration -*- lexical-binding: t; -*-

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

;; Basic setup for Mew
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

;; Hide passwords
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;; Read Mail menu setup for Emacs 21
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

;; Mail user agent setup
(autoload 'mew-user-agent-compose "mew" nil t)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))

(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(define-key global-map "\C-c\C-c\ m" 'mew)

;;; mew-settings.el ends here
