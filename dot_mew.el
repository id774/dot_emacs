;; HTML メール
(require 'mew-w3m)
(defadvice mew-summary-display (after mew-auto-analize-again activate)
(mew-summary-analyze-again))
;; 起動時にメール取得しない
(setq mew-auto-get nil)
;; POP サーバーからメールを削除する/しない
(setq mew-pop-delete t)
;; パスワードキャッシュ
(setq mew-use-cached-passwd t)
(setq mew-use-master-passwd t)
;; メール定期受信
(setq mew-use-biff t)
(setq mew-use-biff-bell nil)
(setq mew-pop-biff-interval 5)
;; メールサーバー設定
(setq mew-ssl-cert-directory "/etc/ssl/certs")
(setq mew-ssl-verify-level 0)
(setq mew-config-alist '(
   ("default"
   ("pop-ssl"        . t)
   ("pop-ssl-port"   . "995")
   ("pop-auth"       . pass)
   ("inbox-folder"   . "+inbox-gmail")
   ("name"           . "xxxxxx") 
   ("user"           . "xxxxxx")
   ("mail-domain"    . "gmail.com")
   ("pop-user"       . "xxxxxx@gmail.com")
   ("pop-server"     . "pop.gmail.com")
   ("smtp-ssl"       . t)
   ("smtp-ssl-port"  . "465")
   ("smtp-user"      . "xxxxxx@gmail.com")
   ("smtp-server"    . "smtp.gmail.com"))))
