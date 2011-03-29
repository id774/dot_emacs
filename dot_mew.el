;; HTML メール
(require 'mew-w3m)
(defadvice mew-summary-display (after mew-auto-analize-again activate)
(mew-summary-analyze-again))
;; 起動時にメール取得しない
(setq mew-auto-get nil)
;; POP サーバーからメールを削除する/しない
(setq mew-pop-delete nil)
;; メールサーバー設定
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
