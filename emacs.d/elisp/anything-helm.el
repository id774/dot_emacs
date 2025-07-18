;; anything-helm.el

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
  ;; 自動補完を無効
  (custom-set-variables '(helm-ff-auto-update-initial-value nil))
  ;; C-h でバックスペースと同じように文字を削除
  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-h") 'delete-backward-char)
    (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
    ;; TAB で任意補完
    (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
    (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))
  ;; helm 有効時でも以下の関数はデフォルトのものを使用
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

