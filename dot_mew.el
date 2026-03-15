;; HTML mail support
(require 'mew-w3m)

(defadvice mew-summary-display
  (after mew-auto-analize-again activate)
  (mew-summary-analyze-again))

;; Fetch mail automatically at startup (enable/disable)
(setq mew-auto-get nil)

;; Delete mail from POP server (enable/disable / number of days)
(setq mew-pop-delete 90)

;; Password cache
(setq mew-use-cached-passwd t)
(setq mew-use-master-passwd t)

;; Periodic mail check
(setq mew-use-biff t)
(setq mew-use-biff-bell nil)
(setq mew-pop-biff-interval 5)

;; Mail server settings
(setq mew-ssl-cert-directory "/etc/ssl/certs")
(setq mew-ssl-verify-level 2)

(setq mew-config-alist
      '(
        ;; Gmail
        ("default"
         ("pop-ssl"       . t)
         ("pop-ssl-port"  . "995")
         ("pop-auth"      . pass)
         ("inbox-folder"  . "+inbox-gmail")
         ("name"          . "xxxxxx")
         ("user"          . "xxxxxx")
         ("mail-domain"   . "gmail.com")
         ("pop-user"      . "xxxxxx@gmail.com")
         ("pop-server"    . "pop.gmail.com")
         ("smtp-ssl"      . t)
         ("smtp-ssl-port" . "465")
         ("smtp-user"     . "xxxxxx@gmail.com")
         ("smtp-server"   . "smtp.gmail.com"))

        ;; POP
        ; ("default"
        ;  ("pop-auth"      . pass)
        ;  ("mew-pop-port"  . "110")
        ;  ("inbox-folder"  . "+inbox-gmail")
        ;  ("name"          . "xxxxxx")
        ;  ("user"          . "xxxxxx")
        ;  ("mail-domain"   . "xxx.co.jp")
        ;  ("pop-user"      . "xxxxxx")
        ;  ("pop-server"    . "xxx.co.jp")
        ;  ("smtp-user"     . "xxxxxx")
        ;  ("smtp-server"   . "xxx.co.jp")
        ;  ("smtp-auth"     . pass)
        ;  ("mew-smtp-port" . "25"))
        ))
