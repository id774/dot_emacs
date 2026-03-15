;;; riece-navi2ch.el --- Short description -*- lexical-binding: t; -*-

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

;; riece
(when (autoload-p 'riece "riece" "Riece IRC Client for Emacs" 'interactive)
  (setq riece-channel-list-buffer-mode t)
  (setq riece-user-list-buffer-mode t)
  (setq riece-layout "spiral")

  (setq riece-addons
        '(;; riece-alias
          riece-biff
          riece-button
          riece-ctcp
          riece-ctlseq
          riece-foolproof
          riece-guess
          riece-highlight
          riece-history
          riece-icon
          riece-ignore
          riece-keepalive
          riece-keyword
          riece-menu
          riece-shrink-buffer
          riece-toolbar
          riece-unread
          riece-url
          riece-xface
          riece-yank))

  (setq riece-max-buffer-size 8192)
  (setq riece-gather-channel-modes t)
  (setq riece-buffer-dispose-function nil) ;; 'kill-buffer
  (setq riece-ignore-discard-message t)
  (setq riece-default-coding-system 'utf-8)
  (setq riece-ctlseq-hide-controls t) ;; t or nil

  (setq riece-ctlseq-colors
        '("white" "black" "DarkBlue" "DarkGreen"
          "red" "maroon" "purple" "orange"
          "yellow" "green" "DarkCyan" "cyan"
          "blue" "magenta" "gray" "DimGray")))

;;   (setq riece-server "ircnet"
;;         riece-server-alist '(("ircnet" :host "irc.tokyo.wide.ad.jp")
;;                              ("freenode" :host "chat.freenode.net"))
;;         riece-startup-server-list '("ircnet")
;;         riece-startup-channel-list '("#nadoka:*.jp ircnet"
;;                                      "#rrr:*.jp ircnet"))
;;
;;   (add-hook 'riece-keyword-notify-functions
;;             (lambda (keyword message)
;;               (write-region
;;                (riece-format-message message t)
;;                nil "/dev/shm/riece.touch" 0)))
;;
;;   (add-hook 'riece-after-switch-to-channel-functions
;;             (lambda (last)
;;               (call-process "rm" nil nil nil "/dev/shm/riece.touch")))
;;
;;   (add-hook 'riece-keyword-notify-functions
;;             (lambda (keyword message)
;;               (let ((ring-bell-function nil)
;;                     (visible-bell t))
;;                 (ding)))))

(load-p "italk")

;; navi2ch
(when (autoload-p 'navi2ch "navi2ch" "navi2ch" 'interactive)
  (setq navi2ch-list-bbstable-url "http://menu.2ch.net/bbsmenu.html")
  (setq navi2ch-article-auto-range nil)
  (setq navi2ch-mona-enable t)

  ;; Use proxy settings from init.el
  (if global-proxy-use
      (defvar navi2ch-net-http-proxy
        (concat global-proxy-server ":" (number-to-string global-proxy-port)))
    (defvar navi2ch-net-http-proxy-userid global-proxy-user)
    (defvar navi2ch-net-http-proxy-password global-proxy-password)))

;;; riece-navi2ch.el ends here
