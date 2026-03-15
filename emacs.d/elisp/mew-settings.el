;; mew
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
