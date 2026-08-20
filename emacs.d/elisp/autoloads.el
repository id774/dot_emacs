;;; autoloads.el --- Load and initialize DOT_EMACS configuration modules -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve supported behavior and maintain backward compatibility.
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

;; mew
(load-p "mew-settings")

;; auto-complete
(load-p "auto-complete-settings")

;; recentf-ext
(load-p "recentf-ext-settings")

;; multi-term
(load-p "multi-term-settings")

;; open-junk-file
(load-p "open-junk-file")

;; paredit
(load-p "paredit-settings")

;; auto async byte compile
(load-p "auto-async-settings")

;; timidity
(autoload-p 'timidity "timidity" "TiMidity++" 'interactive)

;; language modes
(load-p "lang-mode")

;; yatex
(load-p "yatex-mode")

;; anything-git-files
(when (require 'anything-git-files nil t)
  (define-key global-map "\C-c\C-c\ b" 'anything-git-files))

;; sense-region
(when (autoload-p 'sense-region-on "sense-region" "sense-region" 'interactive)
  (sense-region-on))

;; emacs-w3m
(load-p "emacs-w3m")

;; italk is not bundled, so stay quiet when it is absent
(load-p "italk" t)

;; mic-paren (not bundled, so stay quiet when it is absent)
(when (load-p "mic-paren" t)
  (paren-activate))

;; develock (not bundled, so stay quiet when it is absent)
(when (load-p "develock" t)
  (global-font-lock-mode t))

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
(when (require 'wdired nil t)
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;; browse-kill-ring
(when (require 'browse-kill-ring nil t)
  (global-set-key (kbd "C-c k") 'browse-kill-ring))

;; Minibuffer completion navigation.
;; GNU Emacs 30+ provides visible completion navigation natively;
;; older versions keep using the historical zlc implementation.
(if (boundp 'minibuffer-visible-completions)
    (setq minibuffer-visible-completions t)
  (load-p "zlc-settings"))

;; uniquify
(when (load-p "uniquify")
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; google-this
(load-p "google-this-settings")

;; diminish
(load-p "diminish-settings")

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

;; whitespace
(load-p "whitespace-settings")

;; hlinum is retained for GNU Emacs 23.4 through 28.x.
(when (< emacs-major-version 29)
  (load-p "hlinum"))

;; persistent-scratch
(load-p "persistent-scratch")

;; anything
(load-p "anything-settings")

;; key-chord
;; http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el
(when (load-p "key-chord")
  (setq key-chord-two-keys-delay 0.02)
  (key-chord-mode 1))

;; kill-ring persistence across sessions
(require 'savehist)
(add-to-list 'savehist-additional-variables 'kill-ring)
(savehist-mode 1)

;; clear-kill-ring
(load-p "clear-kill-ring")

(load-p "auto-save-buffers-settings")

;; dired-async
(when (and (require 'cl-lib nil t)
           (load-p "dired-async"))
  (dired-async-mode 1))

;; faces
(load-p "faces")

;; configs (loaded last)
(load-p "configs")

;; site-lisp loader (optional, so do not report its absence)
(load-p "loader" t)

;;; autoloads.el ends here
