;; view-mode-key.el
;; Custom behavior for view-mode (read-only navigation)

;; References
;; http://d.hatena.ne.jp/rubikitch/20081104/1225745862
;; http://d.hatena.ne.jp/yaotti/20081104/1225809687
;; http://gist.github.com/22143

(require 'viewer)

;; Keep view-mode active when switching buffers
(viewer-stay-in-setup)

;; Override RET behavior in view-mode for specific major modes
(define-overriding-view-mode-map c-mode
  ("RET" . gtags-find-tag-from-here))

(define-overriding-view-mode-map emacs-lisp-mode
  ("RET" . find-function-at-point))

;; Open read-only files automatically in view-mode
(setq view-read-only t)

;; Keybindings for pager-like navigation in view-mode
(defvar pager-keybind
  `( ;; vi-like navigation
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
    ("v" . scroll-other-window)))

;; Utility: define multiple keys from a key-table
(defun define-many-keys (keymap key-table &optional includes)
  (let (key cmd)
    (dolist (key-cmd key-table)
      (setq key (car key-cmd)
            cmd (cdr key-cmd))
      (if (or (not includes) (member key includes))
          (define-key keymap key cmd))))
  keymap)

;; view-mode initialization
(defun view-mode-hook0 ()
  ;; Apply pager-style keybindings
  (define-many-keys view-mode-map pager-keybind)

  ;; Highlight current line
  (hl-line-mode 1)

  ;; Ensure space scrolls forward
  (define-key view-mode-map " " 'scroll-up))

(add-hook 'view-mode-hook 'view-mode-hook0)

;; Open non-writable files using view-mode automatically
(defadvice find-file
  (around find-file-switch-to-view-file (file &optional wild) activate)
  (if (and (not (file-writable-p file))
           (not (file-directory-p file)))
      (view-file file)
    ad-do-it))

;; Prevent leaving view-mode for read-only files
(viewer-stay-in-setup)

(provide 'view-support)
