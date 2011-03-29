;; HTML $B%a!<%k(B
(require 'mew-w3m)
(defadvice mew-summary-display (after mew-auto-analize-again activate)
(mew-summary-analyze-again))
;; $B5/F0;~$K%a!<%k<hF@$7$J$$(B
(setq mew-auto-get nil)
;; POP $B%5!<%P!<$+$i%a!<%k$r:o=|$9$k(B/$B$7$J$$(B
(setq mew-pop-delete nil)
;; $B%a!<%k%5!<%P!<@_Dj(B
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
