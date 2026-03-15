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
