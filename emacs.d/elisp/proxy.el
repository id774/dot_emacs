;;; proxy.el --- Global proxy configuration for network access -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; Proxy configuration

;; Set `global-proxy-use' to t to enable the proxy
(defvar global-proxy-use nil)

(defvar global-proxy-server "proxy.hoge.co.jp")
(defvar global-proxy-port 8080)
(defvar global-proxy-user nil)
(defvar global-proxy-password nil)

;;; proxy.el ends here
