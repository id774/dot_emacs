;;; autoloads.el --- Short description -*- lexical-binding: t; -*-

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

;; autoloads.el
;; Load configuration modules and call configs.el at the end

(load "utils")

;; core compatibility
(load-p "core-compat-bridge")

;; cl / cl-lib compatibility
(load-p "cl-compat-bridge")

;; ESS compatibility
(load-p "ess-compat-bridge")

;; mew
(load-p "mew-settings")

;; default major mode
(setq default-major-mode 'text-mode)

;; auto-complete
(load-p "auto-complete-settings")

;; smartchr
(load-p "smartchr-settings")

;; recentf-ext
(load-p "recent-ext-settings")

;; multi-term
(load-p "multi-term-settings")

;; git
(load-p "git")
(load-p "git-blame")

;; open-junk-file
(load-p "open-junk-file")

;; paredit
(load-p "paredit-settings")

;; auto async byte compile
(load-p "auto-async-settings")

;; timidity
(when (autoload-p 'timidity "timidity" "TiMidity++" 'interactive))

;; language modes
(load-p "lang-mode")

;; yatex
(load-p "yatex-mode")

;; anything-git-files
(when (require 'anything-git-files)
  (define-key global-map "\C-c\C-c\ b" 'anything-git-files))

;; sense-region
(when (autoload-p 'sense-region-on "sense-region" "sense-region" 'interactive)
  (sense-region-on))

;; emacs-w3m
(load-p "emacs-w3m")

;; riece / navi2ch
(load-p "riece-navi2ch")

;; mic-paren
(when (load-p "mic-paren")
  (paren-activate))

;; develock
(when (load-p "develock")
  (global-font-lock-mode t))

;; physical-line
(load-p "physical-line")

;; windmove
(when (load-p "windmove")
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; screen
(load-p "screen")

;; popwin
(load-p "popwin-el")

;; dired
(load-p "dired-settings")

;; wdired
(when (require 'wdired)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;; minibuf-isearch
(require 'minibuf-isearch)

;; browse-kill-ring
(when (require 'browse-kill-ring)
  (global-set-key (kbd "C-c k") 'browse-kill-ring))

;; zlc
(load-p "zlc-settings")

;; auto-save-buffers
(when (load-p "auto-save-buffers")
  (setq auto-save-buffers-regexp "^/[^:]+/")
  (run-with-idle-timer 0.1 t 'auto-save-buffers))

;; uniquify
(when (load-p "uniquify")
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; google-this
(load-p "google-this-settings")

;; diminish
(load-p "diminish-settings")

;; shadow
(load-p "shadow-settings")

;; tramp
(load-p "tramp-settings")

;; redo
(load-p "redo-settings")

;; cua rectangle
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; disable CUA keybindings

;; kill-all-buffers
(load-p "kill-all-buffers")

;; delete-empty-file
(load-p "delete-empty-file")

;; tab width
(load-p "tab4")

;; fuzzy-format
(require 'fuzzy-format)
(setq fuzzy-format-default-indent-tabs-mode nil)
(global-fuzzy-format-mode t)

;; jaspace-mode
(when (load-p "jaspace-mode")
  ;;(setq jaspace-alternate-jaspace-string "□")
  (setq jaspace-alternate-eol-string "$\n")
  (setq jaspace-highlight-tabs t))

;; hlinum
(cond
 ((>= emacs-major-version '23)
  (load-p "hlinum")))

;; new-file-p
;(load-p "new-file-p")

;; persistent-scratch
(load-p "persistent-scratch")

;; ESS
(when (and (>= emacs-major-version 24)
           (<= emacs-major-version 26))
  (load-p "ess-site"))

;; anything / helm
(load-p "anything-helm")

;; key-chord
;; http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el
(when (load-p "key-chord")
  (setq key-chord-two-keys-delay 0.02)
  (key-chord-mode 1))

;; minor-mode priority
(load-p "minor-mode-hack")
;;(lower-minor-mode-map-alist 'ruby-electric-mode)
;;(raise-minor-mode-map-alist 'anthy-minor-mode)

;; highlight-unique-symbol
;; http://hitode909.hatenablog.com/entry/2013/02/11/233449
(when (require 'highlight-unique-symbol)
  (highlight-unique-symbol t))

;; foreign-regexp
(load-p "foreign-regexp-settings")

;; savekill
(require 'savekill)

;; clear-kill-ring
(load-p "clear-kill-ring.el")

(load-p "auto-save-buffers-settings")

;; web-mode
;; http://web-mode.org/
(load-p "web-mode-settings")

;; dired-async
(when (load-p "dired-async-mode")
  (dired-async-mode 1))

;; faces
(load-p "faces")

;; configs (loaded last)
(load-p "configs")

;; site-lisp loader
(load-p "loader")

;;; autoloads.el ends here
