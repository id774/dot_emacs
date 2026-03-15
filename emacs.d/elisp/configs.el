;;; configs.el --- Core environment configuration for DOT_EMACS -*- lexical-binding: t; -*-

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

;; Basic environment settings

;; Show the current function in the mode line
(which-func-mode)

;; blink
(blink-cursor-mode nil)

;; Hide tool bar and scroll bar
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;; menu-bar
(menu-bar-mode -1)

;; mouse-wheel
;; Disabled because it does not work on Emacs 23.2
;;(mouse-wheel-mode 1)

;; xterm / gnome-terminal
(xterm-mouse-mode -1)

;; ;; fringe
;; (fringe-mode 8)

;; ;; display-time
(display-time)

;; line-number / column-number
(line-number-mode t)
(column-number-mode t)

;; Use spaces for indentation by default
(setq-default tab-width 4 indent-tabs-mode nil)

;; image display
(auto-image-file-mode)

;; auto-save files
(setq auto-save-default nil)

;; backup files
(setq make-backup-files t)

;; backup directory
;; Safer even when files are under version control
;; Backups are saved as !path!to!file-name~
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

;; transient-mark
(setq transient-mark-mode t)

;; isearch highlighting
(setq search-highlight t)
(setq query-replace-highlight t)
;;(setq isearch-lazy-highlight-initial-delay 0) ; obsolete
(setq lazy-highlight-initial-delay 0)

;; woman
(setq woman-manpath '("/usr/local/man"
                      "/usr/share/man"
                      "/usr/local/share/man"
                      "/sw/man"
                      "/usr/share/man/ja_JP.ujis"))
(setq woman-cache-filename (expand-file-name "~/.emacs.d/woman-cache"))

;; Do not change inode numbers when creating backups
(setq backup-by-copying t)

;; GC threshold
(setq gc-cons-threshold 1000000)

;; Disable the splash screen for faster startup
(setq inhibit-startup-message t)

;; visible-bell
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Do not limit font-lock by file size
(setq font-lock-maximum-size nil)

;; ;; fast-lock
;; (setq font-lock-support-mode 'fast-lock-mode)
;; (setq fast-lock-cache-directories '("~/.emacs.d/emacs-flc"))

;; auto-save-list
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")

;; Always add a trailing newline
(setq require-final-newline t)

;; temporary-file-directory
;; (setq temporary-file-directory "~/.emacs.d/tmp")
(setq temporary-file-directory "/dev/shm")

;; Scroll one line at a time
(setq scroll-conservatively 1)

;; Do not create new lines at end of buffer
;; Default on Emacs 21
(setq next-line-add-newlines nil)

;; fill-column
(setq fill-column 79)

;; *Messages* size
(setq message-log-max 200)

;; abbrev file handling
(cond
 ((>= emacs-major-version 28)
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs"))
 (t
  (setq save-abbrevs nil)
  (setq abbrev-file-name "/dev/null")))
(setq-default abbrev-mode nil)

;; auto-compression
(auto-compression-mode t)

;; apropos
(setq apropos-do-all t)

;; abbrev
;; (read-abbrev-file "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)

;; version control
(setq vc-follow-symlinks t)
(setq vc-suppress-confirm t)
(setq vc-command-messages t)

;; narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Use a single cursor
(setq cursor-in-non-selected-windows nil)

;; Highlight empty lines
(setq-default indicate-empty-lines t)

;; Let C-k at beginning of line kill the whole line
(setq kill-whole-line t)

;; line-spacing
;; (setq-default line-spacing 0)

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "")

;; paren-mode
(show-paren-mode 1)
(find-function-setup-keys)

;; Anthy
;; (set-input-method "japanese-anthy")
;; (set-input-method "japanese-prime")

;; C language settings

;; Ruby default style
(c-add-style "ruby"
             '("bsd"
               (c-offsets-alist
                (case-label . 2)
                (label . 2)
                (statement-case-intro . 2))))

;; Stroustrup style
(defun-add-hook 'c-mode-common-hook
  (c-set-style "Stroustrup")
  (c-toggle-hungry-state 1)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

;; Highlight the current line
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline) ; underline
(hl-line-mode 1)

;; Start in view-mode after opening a file (toggle with C-x j)
(add-hook 'find-file-hooks
          (lambda ()
            (cond (view-mode)
                  (t
                   (view-mode)))
            ;; line numbers (toggle with C-x t)
            (cond
             ((>= emacs-major-version '23)
              (linum-mode)
              (custom-set-variables
               '(global-linum-mode t)))
             ((< emacs-major-version '23)
              (require 'wb-line-number)
              (custom-set-faces
               '(wb-line-number-face ((t (:foreground "LightGrey"))))
               '(wb-line-number-scroll-bar-face
                 ((t (:foreground "white" :background "LightBlue2")))))))))

;; Wrap at window edge
(setq truncate-partial-width-windows nil)

;; fullscreen
;; Uncomment to start maximized
;(set-frame-parameter nil 'fullscreen 'fullboth)
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen)
                           nil
                         'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; frame transparency
(add-to-list 'default-frame-alist '(alpha . (90 75)))

;; global key bindings
(load-p "global-set-key")

;; view-mode key bindings
(load-p "view-mode-key")

;; key-chord key bindings
(load-p "key-chord-define-global")

;; Open root-owned files via TRAMP sudo
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only and owner is root. Open it with sudo? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; Use y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Platform-specific settings
(if window-system
    (progn
      (cond
       ((eq system-type 'windows-nt))
       ((eq system-type 'gnu/linux)
        (setenv "JAVA_HOME" "/usr/lib/jvm/java-6-sun")
        ;; Mozc
        (require 'mozc)
        (setq default-input-method "japanese-mozc")
        (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method))
       ((eq system-type 'darwin)
        (setenv "JAVA_HOME" "/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Home")
        (cond
         ((< emacs-major-version '23)
          (progn
            (set-frame-parameter nil 'fullscreen 'fullboth) ; maximize
            ))
         ((>= emacs-major-version '23)
          (progn
            (tool-bar-mode 0) ; hide toolbar
            )))))))

;;; configs.el ends here
