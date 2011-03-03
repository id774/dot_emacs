;; http://d.hatena.ne.jp/rubikitch/20081104/1225745862
;; http://d.hatena.ne.jp/yaotti/20081104/1225809687
;; http://gist.github.com/22143

(require 'viewer)
(viewer-stay-in-setup)
(define-overriding-view-mode-map c-mode
  ("RET" . gtags-find-tag-from-here))
(define-overriding-view-mode-map emacs-lisp-mode
  ("RET" . find-function-at-point))

(setq view-read-only t)
(defvar pager-keybind
      `( ;; vi-like
        ("h" . backward-word)
        ("l" . forward-word)
        ("j" . next-line)
        ("k" . previous-line)
        ("J" . next-window-line)
        ("K" . previous-window-line)
        ("b" . scroll-down)
        ("f" . scroll-up)
        (" " . scroll-up)
        ("w" . forward-word)
        ("e" . backward-word)
        ("n" . ,(lambda () (interactive) (scroll-up 1)))
        ("p" . ,(lambda () (interactive) (scroll-down 1)))
        ("[" . forward-sexp)
        ("]" . backward-sexp)
        ("." anything-c-moccur-occur-by-moccur)
        ("c" . scroll-other-window-down)
        ("v" . scroll-other-window)
        ))
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
        (define-key keymap key cmd))))
  keymap)

(defun view-mode-hook0 ()
  (define-many-keys view-mode-map pager-keybind)
  (hl-line-mode 1)
  (define-key view-mode-map " " 'scroll-up))
(add-hook 'view-mode-hook 'view-mode-hook0)

;; 書き込み不能なファイルはview-modeで開くように
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))

;; 書き込み不能な場合はview-modeを抜けないように
(viewer-stay-in-setup)

(provide 'view-support)
