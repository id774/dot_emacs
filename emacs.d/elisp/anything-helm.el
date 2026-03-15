;;; anything-helm.el --- Short description -*- lexical-binding: t; -*-

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

;; Provide with-eval-after-load for Emacs versions prior to 24.4
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       '(progn ,@body))))

(cond
 ;; Helm
 ((and (>= emacs-major-version 24)
       (<= emacs-major-version 26)
       (require 'helm-config nil 'noerror))
  (helm-mode 1)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key global-map "\C-x\C-b" 'helm-for-files)
  (global-set-key (kbd "C-;") 'helm-find-files)
  (global-set-key (kbd "C-:") 'helm-mini)

  ;; Disable auto completion
  (custom-set-variables '(helm-ff-auto-update-initial-value nil))

  ;; Make C-h behave like backspace
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

    ;; Use TAB for persistent action (arbitrary completion)
    (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))

  ;; Use default behavior for these functions even when helm is enabled
  (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil)))

 ;; Anything
 ((and (>= emacs-major-version 23)
       (load-p "anything-config"))
  (setq anything-sources
        (list anything-c-source-buffers
              anything-c-source-bookmarks
              anything-c-source-recentf
              anything-c-source-file-name-history
              anything-c-source-locate))

  (define-key global-map "\C-x\ b" 'anything)

  (with-eval-after-load 'anything
    (define-key anything-map (kbd "C-p") 'anything-previous-line)
    (define-key anything-map (kbd "C-n") 'anything-next-line)
    (define-key anything-map (kbd "C-v") 'anything-next-source)
    (define-key anything-map (kbd "M-v") 'anything-previous-source)))

 ;; fallback: minimal buffer switcher
 (t
  (define-key global-map "\C-x\C-b" 'electric-buffer-list)))

;;; anything-helm.el ends here
