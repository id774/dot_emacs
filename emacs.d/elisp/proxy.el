;;; proxy.el --- Global proxy configuration for network access -*- lexical-binding: t; -*-

;; Author: id774 (More info: https://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Policy  : Preserve supported behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; This file owns shared environment-level proxy configuration for DOT_EMACS.
;; Environments that require a proxy set these values explicitly before
;; network integrations are loaded, normally through the install-time local
;; configuration override.  This is not a live reconfiguration interface.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; Proxy configuration
;; These variables are the shared proxy configuration owned by this file.
;; Consumers may read them during their own load/setup.  Changing them later
;; does not by itself reload or reconfigure an already-loaded integration.

;; Set `global-proxy-use' to t in environments that require the proxy
(defvar global-proxy-use nil)

(defvar global-proxy-server "proxy.hoge.co.jp")
(defvar global-proxy-port 8080)
(defvar global-proxy-user nil)
(defvar global-proxy-password nil)

;;; proxy.el ends here
