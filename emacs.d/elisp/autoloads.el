;; autoloads.el
;; 外部ファイルをロードして configs.el を最後に呼ぶ

(load "utils")

;; Mew
(load-p "mew-settings")

;; Text モードをデフォルトにする
(setq default-major-mode 'text-mode)

;; オートコンプリート
(load-p "auto-complete-settings")

;; 同じキーバインド連打でラクをする
(load-p "smartchr-settings")

;; recentf-ext
(load-p "recent-ext-settings")

;; multi-term
(load-p "multi-term-settings")

;; git.el
(load-p "git")
(load-p "git-blame")

;; open-junk-file
(load-p "open-junk-file")

;; paredit
(load-p "paredit-settings")

;; auto-async-byte-compile
(load-p "auto-async-settings")

;; timidity-mode : TiMidity++ emacs front-end
(when (autoload-p 'timidity "timidity" "TiMidity++" 'interactive))

;; Lang modes
(load-p "lang-mode")

;; YaTeX modes
(load-p "yatex-mode")

;; anything-git-files
(when (require 'anything-git-files)
  (define-key global-map "\C-c\C-c\ b" 'anything-git-files))

;; sense-region.el : \C-spc で region<->rectabgle をトグル
(when (autoload-p 'sense-region-on "sense-region" "sense-region" 'interactive)
  (sense-region-on))

;; emacs-w3m
(load-p "emacs-w3m")

;; riece & navi2ch
(load-p "riece-navi2ch")

;; 括弧強調
(when (load-p "mic-paren")
  (paren-activate))

;; 色つき
(when (load-p "develock")
  (global-font-lock-mode t))

;; 物理行移動
(load-p "physical-line")

(when (load-p "windmove")
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; screen の hard status を更新する
(load-p "screen")

;; popwin-el
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

;; zsh like completion
(load-p "zlc-settings")

;; 自動保存
(when (load-p "auto-save-buffers")
  (setq auto-save-buffers-regexp "^/[^:]+/")
  (run-with-idle-timer 0.1 t 'auto-save-buffers))

(when (load-p "uniquify")
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; google
(load-p "google-this-settings")

;; diminish
(load-p "diminish-settings")

;; shadow.el
(load-p "shadow-settings")

;; TRAMP
(load-p "tramp-settings")

;; Redo
(load-p "redo-settings")

;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止

;; 開いているすべてのバッファを kill する
(load-p "kill-all-buffers")

;; ファイルの内容が無ければ、ファイルとバッファを消す
(load-p "delete-empty-file")

;; タブを 4 に
(load-p "tab4")

;; fuzzy-format
(require 'fuzzy-format)
(setq fuzzy-format-default-indent-tabs-mode nil)
(global-fuzzy-format-mode t)

;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (load-p "jaspace-mode")
  ;;(setq jaspace-alternate-jaspace-string "□")
  (setq jaspace-alternate-eol-string "$\n")
  (setq jaspace-highlight-tabs t)
)

;; hlinum-mode
(cond
  ((>= emacs-major-version '23)
    (load-p "hlinum")))

;; 新しいファイルを作る前に確認
;(load-p "new-file-p")

;; scratch バッファを消しても再生成する
(load-p "persistent-scratch")

;; ESS
(load-p "ess-site")

;; Anything.el & Helm
(load-p "anything-helm")

;; key-chord.el 複数キー同時押しをサポート
;; http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el
(when (load-p "key-chord")
  (setq key-chord-two-keys-delay 0.02)
  (key-chord-mode 1))

;; Customize minor-mode key priority
(load-p "minor-mode-hack")
;;(lower-minor-mode-map-alist 'ruby-electric-mode)
;;(raise-minor-mode-map-alist 'anthy-minor-mode)

;; ユニークなシンボルをハイライトする
;; http://hitode909.hatenablog.com/entry/2013/02/11/233449
(when (require 'highlight-unique-symbol)
  (highlight-unique-symbol t))

;; Perl 風の正規表現
(load-p "foreign-regexp-settings")

;; Kill Ring を保存する
(require 'savekill)

;; web-mode
;; http://web-mode.org/
(load-p "web-mode-settings")

;; 非同期にファイルをコピー
(when (load-p "dired-async-mode")
  (dired-async-mode 1))

;; 表示設定をロードする
(load-p "faces")

;; 環境設定をロードする (最後に)
(load-p "configs")

;; ~/.emacs.d/site-lisp をすべてロードする (configs のさらに後)
(load-p "loader")

