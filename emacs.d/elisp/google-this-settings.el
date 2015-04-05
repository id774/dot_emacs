(require 'google-this)
;; (setq google-this-location-suffix "co.jp")
(defun google-this-url () "URL for google searches."
  (concat google-this-base-url google-this-location-suffix
          "/search?q=%s&hl=ja&num=10&as_qdr=y5&lr=lang_ja"))
(global-set-key (kbd "M-g M-g") 'google-this)
