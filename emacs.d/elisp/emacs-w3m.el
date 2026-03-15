;;; emacs-w3m.el --- Configure emacs-w3m browser integration -*- lexical-binding: t; -*-

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

;; emacs-w3m
(when (autoload-p 'w3m "w3m" "Interface for w3m on Emacs." 'interactive)
  (autoload 'w3m-find-file  "w3m"        "Find a local file using emacs-w3m." t)
  (autoload 'w3m-browse-url "w3m"        "Ask a WWW browser to show a URL."   t)
  (autoload 'w3m-search     "w3m-search" "Search words using emacs-w3m."      t)
  (autoload 'w3m-weather    "w3m-weather" "Display a weather report."         t)
  (autoload 'w3m-antenna    "w3m-antenna" "Report changes of web sites."      t)
  (autoload 'w3m-namazu     "w3m-namazu"  "Search files with Namazu."         t)

  ;; w3m settings
  (setq w3m-use-cookies t)
  (setq w3m-cookie-accept-bad-cookies nil)

  ;; Use w3m as the default browser
  (setq browse-url-browser-function 'w3m-browse-url)
  (global-set-key "\C-xm" 'browse-url-at-point)

  ;; Use proxy settings from init.el
  (if global-proxy-use
      (setq w3m-command-arguments-alist
            '(("^http://\\([^/]*\\.\\)hoge\\.co\\.jp\\(/\\|$\\)" "-no-proxy")
              ;; Use the proxy server for other URLs
              ("" "-o" "http_proxy=http://proxy.hoge.co.jp:8080/"))))

  ;; key bindings
  (define-key global-map "\C-c\C-c\C-l" 'w3m)
  (define-key global-map "\C-c\C-c\ l"  'w3m))

;;; emacs-w3m.el ends here
