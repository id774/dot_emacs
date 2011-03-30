;; Mew を使う為の設定
(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
;; パスワード非表示
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
;; Optional setup (Read Mail menu for Emacs 21):
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))
;; Optional setup (e.g. C-xm for sending a message):
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


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
