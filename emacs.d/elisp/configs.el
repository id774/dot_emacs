;; configs.el
;; 基本的な環境設定

;; モードラインに今いる関数を表示
(which-func-mode)

;; blink
(blink-cursor-mode nil)

;; hide tool-bar and scroll-bar
(if window-system
  (progn
    (tool-bar-mode -1)
    (scroll-bar-mode -1)))

;; メニューバー使う
(menu-bar-mode -1)

;; ホイールマウス使う
;; emacs 23.2 で使えないのでコメントアウト
;;(mouse-wheel-mode 1)

;; xtermとかgnome-terminalとか
(xterm-mouse-mode -1)

;; ;; fringe(左右に余白のように見えてるアレ)
;; (fringe-mode 8)

;; ;; 時間表示
(display-time)

;; 行番号と列番号を表示
(line-number-mode t)
(column-number-mode t)

;; デフォルトでインデントをスペースに
(setq-default tab-width 4 indent-tabs-mode nil)

;; 画像展開
(auto-image-file-mode)

;; 自動セーブファイルを作成するかどうか
(setq auto-save-default nil)

;; バックアップファイルを作成するかどうか
(setq make-backup-files t)

;; バックアップファイルの保存位置指定
;; VCS で管理していても設定しておくと安全
;; !path!to!file-name~ で保存される
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

;; transient-mark
(setq transient-mark-mode t)

;; isearch を色つきに
(setq search-highlight t)
(setq query-replace-highlight t)
;;(setq isearch-lazy-highlight-initial-delay 0) ; obsolate
(setq lazy-highlight-initial-delay 0)

;; M-x woman
(setq woman-manpath '("/usr/local/man"
                      "/usr/share/man"
                      "/usr/local/share/man"
                      "/sw/man"
                      "/usr/share/man/ja_JP.ujis"))
(setq woman-cache-filename (expand-file-name "~/.emacs.d/woman-cache"))

;; バックアップで inode を変化させないようにする
(setq backup-by-copying t)

;; GC間隔
(setq gc-cons-threshold 1000000)

;; スプラッシュ非表示 : 起動が速くなる
(setq inhibit-startup-message t)

;; 画面反転をやめる
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; あまりに大きいファイルは色付けると時間かかるので、上限を指定する
(setq font-lock-maximum-size nil)

;; ;; fast-lock
;; (setq font-lock-support-mode 'fast-lock-mode)
;; (setq fast-lock-cache-directories '("~/.emacs.d/emacs-flc"))

;; auto-save の場所
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")

;; 最後に改行を付ける
(setq require-final-newline t)

;; 一時ファイルの場所
;; (setq temporary-file-directory "~/.emacs.d/tmp")
(setq temporary-file-directory "/dev/shm")

;; 1行ずつスクロール
(setq scroll-conservatively 1)

;; 新規行を作成しない
;; emacs21ではデフォルト
(setq next-line-add-newlines nil)

;; 80 だとちょっと……
(setq fill-column 79)

;; *Messages* の長さ
(setq message-log-max 200)

;; 略語展開の保存定義
(when (>= emacs-major-version 27)
  (setq save-abbrevs 'silently)   ;; 変更があれば自動保存（確認なし）
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")) ;; 保存先を明示的に指定

;; .gz なファイルとかを透過的に圧縮/伸張
(auto-compression-mode t)

;; apropos をあらゆるとことに
(setq apropos-do-all t)

;; abbrev
;; (read-abbrev-file "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)

;; version control
(setq vc-follow-symlinks t)
(setq vc-suppress-confirm t)
(setq vc-command-messages t)

;; narrowing の警告を抑止
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; カーソル一個
(setq cursor-in-non-selected-windows nil)

;; 空行強調
(setq-default indicate-empty-lines t)

;; 行頭の C-k で行全体を削除
(setq kill-whole-line t)

;; 行間
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

;; C 言語系の設定群

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

;; カーソル行のハイライト
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
;; (setq hl-line-face 'underline) ; 下線
(hl-line-mode 1)

;; ファイルオープン直後は読み取り専用 (C-x j で切替)
(add-hook 'find-file-hooks
  (lambda ()
    (cond (view-mode)
      (t
        (view-mode)))
;; 行数表示(C-x t で切替)
    (cond
      ((>= emacs-major-version '23)
        (linum-mode)
        (custom-set-variables
          '(global-linum-mode t))
      )
      ((< emacs-major-version '23)
        (require 'wb-line-number)
        (custom-set-faces
         '(wb-line-number-face ((t (:foreground "LightGrey"))))
         '(wb-line-number-scroll-bar-face
           ((t (:foreground "white" :background "LightBlue2")))))
      ))))

;; 画面端で折り返す
(setq truncate-partial-width-windows nil)

;; fullscreen
;; 起動時にウィンドウを最大化する場合はコメントアウトを解除
;(set-frame-parameter nil 'fullscreen 'fullboth)
(defun toggle-fullscreen ()
  (interactive)
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
      nil
      'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; 透明化
(add-to-list 'default-frame-alist '(alpha . (95 80)))

;; キーバインド設定
(load-p "global-set-key")

;; view-mode キーバインド設定
(load-p "view-mode-key")

;; key-chord.el 専用キーバインド設定
(load-p "key-chord-define-global")

;; root 所有のファイルを開くときは tramp で sudo する
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

;; yes/no の質問をすべて y/n に変更
(defalias 'yes-or-no-p 'y-or-n-p)

;; その他の環境固有の設定をここに
(if window-system
  (progn
    (cond
      ((eq system-type 'windows-nt)
      )
      ((eq system-type 'gnu/linux)
        (setenv "JAVA_HOME" "/usr/lib/jvm/java-6-sun")
        ;; Mozc
        (require 'mozc)
        (setq default-input-method "japanese-mozc")
        (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method)
      )
      ((eq system-type 'darwin)
        (setenv "JAVA_HOME" "/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Home")
        (cond
          ((< emacs-major-version '23)
            (progn
              (set-frame-parameter nil 'fullscreen 'fullboth) ; 最大化
              ))
          ((>= emacs-major-version '23)
            (progn
              (tool-bar-mode 0) ; toolbar非表示
              ))
        )
      )
    )
  )
)

