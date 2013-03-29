;; autoloads.el
;; 外部ファイルをロードして configs.el を最後に呼ぶ

(load "utils")

;; Mew
(load-p "mew-settings")

;; Text モードをデフォルトに
(setq default-major-mode 'text-mode)

;; オートコンプリート
(when (load-p "auto-complete")
  (global-auto-complete-mode t))

;; git.el
(load-p "git")
(load-p "git-blame")

;; open-junk-file
(load-p "open-junk-file")

;; paredit
(when (load-p "paredit")
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode))

;; auto-async-byte-compile
(when (load-p "auto-async-byte-compile")
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;; timidity-mode : TiMidity++ emacs front-end
(when (autoload-p 'timidity "timidity" "TiMidity++" 'interactive))

(defun ruby-optional-load ()
  ;; ruby-electric.el
  (require 'ruby-electric)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

  ;; rubydbnx.el
  (autoload 'rubydb "rubydb2x"
  "run rubydb on program file in buffer *gud-file*.
  the directory containing file becomes the initial working directory
  and source-file directory for your debugger." t)

  ;; ruby-block.el
  (require 'ruby-block)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

;; ruby-mode
(when (autoload-p 'ruby-mode "ruby-mode" "Ruby" 'interactive)
  (setq auto-mode-alist (cons '("\\.rb$" . ruby-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("ruby" . ruby-mode) interpreter-mode-alist))

  ;; inf-ruby
  (autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
  (autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
  (add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))
  (cond
    ((eq system-type 'gnu/linux)
      (ruby-optional-load)))
  (cond
    ((eq system-type 'darwin)
      (cond
        ((< emacs-major-version '23)
          (ruby-optional-load))))))

;; rd-mode
(when (autoload-p 'rd-mode "RD-mode" "RDtool" 'interactive)
  (setq auto-mode-alist (cons '("\\.rd$" . rd-mode) auto-mode-alist)))

;; php-mode
(when (autoload-p 'php-mode "php-mode" "PHP" 'interactive)
  (setq auto-mode-alist (cons '("\\.php$" . php-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("php" . php-mode) interpreter-mode-alist)))

;; haskell-mode
(when (autoload-p 'haskell-mode "haskell-site-file" "Haskell" 'interactive)
  (setq auto-mode-alist
  (append '(("\\.hs$" . haskell-mode)
      ("\\.hi$" . haskell-mode)
      ("\\.gs$" . haskell-mode)
      ("\\.lhs$" . haskell-mode)
      ("\\.lgs$" . haskell-mode))
    auto-mode-alist))
  (setq interpreter-mode-alist
    (append '(("ruby" . ruby-mode)
    ("hugs" . haskell-mode)
    ("php"  . php-mode))
    interpreter-mode-alist))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))

;; Rinari
(load-p "rinari")

;; rhtml-mode
(when (load-p "rhtml-mode")
  (setq auto-mode-alist (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.rhtml$" . rhtml-mode) auto-mode-alist))
  (add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch))))

;; jsp
(cond
  ((>= emacs-major-version '23)
    (load-p "autostart")))

;; gtags-mode : global 便利。
(when (autoload-p 'gtags-mode "gtags" "GNU GLOBAL" 'interactive)
  (setq gtags-mode-hook
  (function (lambda ()
    (local-set-key "\M-f" 'gtags-find-tag)    ; override etags
    (local-set-key "\M-r" 'gtags-find-rtag)   ; reverse tag
    (local-set-key "\M-s" 'gtags-find-symbol) ; find
    (local-set-key "\C-t" 'gtags-pop-stack)))); pop
  ;; C-mode のときは常に gtags 使用。
  (defun-add-hook 'c-mode-common-hook (gtags-mode 1)))

;; js2-mode
(when (autoload-p 'js2-mode "js2" "js2" 'interactive)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook
            '(lambda ()
               (setq js2-basic-offset 4))))

;; actionscript-mode
(when (require 'actionscript-mode nil t)
  (setq auto-mode-alist
    (cons '("\.as\'" . actionscript-mode) auto-mode-alist)))

;; Zen Coding Mode
(when (load-p "zencoding-mode")
  (add-hook 'sgml-mode-hook 'zencoding-mode)
  (add-hook 'html-mode-hook 'zencoding-mode)
  (add-hook 'text-mode-hook 'zencoding-mode)
  (define-key zencoding-mode-keymap "\C-i" 'zencoding-expand-line))

;; scss-mode
(when (autoload-p 'scss-mode "scss-mode" "scss-mode" 'interactive)
  (setq scss-compile-at-save nil)
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

;; haml-mode
(when (autoload-p 'haml-mode "haml-mode" "haml-mode" 'interactive)
  (add-to-list 'auto-mode-alist '("\\.haml\\'" . haml-mode))
  (add-hook 'haml-mode-hook
    '(lambda ()
      (setq indent-tabs-mode nil)
    )))

;; sass-mode
(when (autoload-p 'sass-mode "sass-mode" "sass-mode" 'interactive)
  (setq sass-compile-at-save nil)
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . sass-mode)))

;; coffee-mode
(when (load-p "coffee-mode")
  (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
  (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode)))

;; bat-mode
(setq auto-mode-alist
       (append 
         (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
         (list (cons "\\.[cC][mM][dD]$" 'bat-mode))
         ;; For DOS init files
         (list (cons "CONFIG\\."   'bat-mode))
         (list (cons "AUTOEXEC\\." 'bat-mode))
         auto-mode-alist))

(autoload-p 'bat-mode "bat-mode"
      "DOS and Windows BAT files" t)

;; sense-region.el : \C-spc で region<->rectabgle をトグル。便利。
(when (autoload-p 'sense-region-on "sense-region" "sense-region" 'interactive)
  (sense-region-on))

;; emacs-w3m
(load-p "emacs-w3m")

;; riece
(when (autoload-p 'riece "riece" "Riece IRC Client for Emacs" 'interactive)
  (setq riece-channel-list-buffer-mode t)
  (setq riece-user-list-buffer-mode t)
  (setq riece-layout "spiral")
  (setq riece-addons
  '(;;riece-alias
    riece-biff
    riece-button
    riece-ctcp
    riece-ctlseq
    riece-foolproof
    riece-guess
    riece-highlight
    riece-history
    riece-icon
    riece-ignore
    riece-keepalive
    riece-keyword
    riece-menu
    riece-shrink-buffer
    riece-toolbar
    riece-unread
    riece-url
    riece-xface
    riece-yank))
  (setq riece-max-buffer-size 8192)
  (setq riece-gather-channel-modes t)
  (setq riece-buffer-dispose-function nil);;'kill-buffer)
  (setq riece-ignore-discard-message t)
  (setq riece-default-coding-system 'utf-8)
  (setq riece-ctlseq-hide-controls t);; t or nil
  (setq riece-ctlseq-colors
  '("white" "black" "DarkBlue" "DarkGreen"
    "red" "maroon" "purple" "orange"
    "yellow" "green" "DarkCyan" "cyan"
    "blue" "magenta" "gray" "DimGray")))
;;   (setq riece-server "ircnet"
;;   riece-server-alist '(("ircnet" :host "irc.tokyo.wide.ad.jp")
;;            ("freenode" :host "chat.freenode.net"))
;;   riece-startup-server-list '("ircnet")
;;   riece-startup-channel-list '("#nadoka:*.jp ircnet"
;;              "#rrr:*.jp ircnet"))
;;   (add-hook 'riece-keyword-notify-functions
;;       (lambda (keyword message)
;;         (write-region
;;          (riece-format-message message t)
;;          nil "/dev/shm/riece.touch" 0)))
;;  (add-hook 'riece-after-switch-to-channel-functions
;;      (lambda (last)
;;        (call-process "rm" nil nil nil "/dev/shm/riece.touch")))
;;  (add-hook 'riece-keyword-notify-functions
;;      (lambda (keyword message)
;;        (let ((ring-bell-function nil)
;;        (visible-bell t))
;;          (ding)))))

(load-p "italk")

(when (autoload-p 'navi2ch "navi2ch" "navi2ch" 'interactive)
  (setq navi2ch-list-bbstable-url "http://menu.2ch.net/bbsmenu.html")
  (setq navi2ch-article-auto-range nil)
  (setq navi2ch-mona-enable t)
  ;; init.el のプロキシ情報を参照
  (if global-proxy-use
    (defvar navi2ch-net-http-proxy (concat global-proxy-server ":" (number-to-string global-proxy-port)))
    (defvar navi2ch-net-http-proxy-userid global-proxy-user)
    (defvar navi2ch-net-http-proxy-password global-proxy-password)))

;; 括弧強調
(when (load-p "mic-paren")
  (paren-activate))

;; 色つき
(when (load-p "develock")
  (global-font-lock-mode t))

;; Open recent。便利。
;; (when (load-p "recentf")
;;  (recentf-mode 1)
;;  ;; Open recent で保存する数
;;  (setq recentf-max-menu-items 16)
;;  (setq recentf-max-saved-items 48))

;; 物理行移動
(load-p "physical-line")

;; なんでもタブでやる ac-mdoe
;; (when (load-p "ac-mode")
;;  (setq ac-mode-exception '(dired-mode hex-mode ruby-mode))
;;  (add-hook 'find-file-hooks 'ac-mode-without-exception))

(when (load-p "windmove")
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))

;; screen と emacs -nw を混ぜるとこいつが強烈にほしくなる
(when (and (not window-system)
     (string-match "^xterm\\|^screen" (getenv "TERM"))
     (load-p "term/xterm"))
  (defun-add-hook 'post-command-hook
    "update terminal hard status."
    (let ((buf (current-buffer)))
      (unless (eq buf hardstatus-update-last-visited)
  (send-string-to-terminal
   (concat "\e]0;"
     (encode-coding-string
      (format-mode-line frame-title-format 0)
      'utf-8 t)
     "\a"))
  (setq hardstatus-update-last-visited buf))))
  (setq hardstatus-update-last-visited nil))
;; (when (and (load-p "xterm-frobs")
;;      (load-p "xterm-title")
;;      (not window-system))
;;      (string-match "^xterm\\|^screen" (getenv "TERM")))
;;   (xterm-title-mode 1))

;; popwin-el
(if (require 'popwin nil t)
    (progn
        (setq display-buffer-function 'popwin:display-buffer)
        (setq popwin:popup-window-height 0.4)
        (setq anything-samewindow nil)
        (push '("*anything*" :height 20) popwin:special-display-config)
        (push '(dired-mode :position top) popwin:special-display-config)
        (push '("\\*[Vv][Cc]" :regexp t :position top) popwin:special-display-config)
        (push '("\\*git-" :regexp t :position top) popwin:special-display-config)
))

;; wdired
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; zsh like completion
(cond
  ((>= emacs-major-version '23)
    (progn
      (require 'zlc)
      (zlc-mode t)
      (setq zlc-select-completion-immediately t)
      (let ((map minibuffer-local-map))
        (define-key map (kbd "<down>")  'zlc-select-next-vertical)
        (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
        (define-key map (kbd "<right>") 'zlc-select-next)
        (define-key map (kbd "<left>")  'zlc-select-previous)
      )
    )
  )
)

;; 自動保存
(when (load-p "auto-save-buffers")
  (setq auto-save-buffers-regexp "^/[^:]+/")
  (run-with-idle-timer 0.1 t 'auto-save-buffers))

(when (load-p "uniquify")
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; diminish
(when (load-p "diminish")
  (diminish 'isearch-mode)
  ;; (diminish 'gtags-mode "G")
  (diminish 'abbrev-mode "Abbr")
  ;; (diminish 'ac-mode "[tab]")
  (diminish 'font-lock-mode ""); 動いていて当たり前
  ;; ついでだから major mode も書き換えちゃえ
  (defun-add-hook 'lisp-interaction-mode-hook (setq mode-name "Lisp"))
  (defun-add-hook 'emacs-lisp-mode-hook (setq mode-name "elisp"))
  (defun-add-hook 'texinfo-mode-hook (setq mode-name "texi"))
  (defun-add-hook 'change-log-mode-hook (setq mode-name "CL")))

;; shadow.el
(when (load-p "shadow")
  (add-hook 'find-file-hooks 'shadow-on-find-file)
  (add-hook 'shadow-find-unshadow-hook
    (lambda () (auto-revert-mode 1))))

;; TRAMP
(when (load-p "tramp")
  (setq tramp-shell-prompt-pattern "^.*[#$%>] *")
  (setq tramp-debug-buffer t)
  (setq tramp-default-method "scpx")
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save")
  (setq tramp-verbose 3)
)

;; Redo
(when (load-p "undo-tree")
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "M-/") 'redo)
)

;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止

;; 開いているすべてのバッファを kill する
(load-p "kill-all-buffers")

;; ファイルの内容が無ければ、ファイルとバッファを消す
(load-p "delete-empty-file")

;; タブを 4 に
(load-p "tab4")

;; タブ, 全角スペース、改行直前の半角スペースを表示する
(load-p "jaspace-mode")
;;(setq jaspace-alternate-jaspace-string "□")
(setq jaspace-alternate-eol-string "$\n")
(setq jaspace-highlight-tabs t)

;; hlinum-mode
(cond
  ((>= emacs-major-version '23)
    (load-p "hlinum")))

;; 新しいファイルを作る前に確認
;(load-p "new-file-p")

;; scratch バッファを消しても再生成する
(load-p "persistent-scratch")

;; Anything.el
(when (load-p "anything-config")
  (setq anything-sources (list anything-c-source-buffers
                               anything-c-source-bookmarks
                               anything-c-source-recentf
                               anything-c-source-file-name-history
                               anything-c-source-locate))
  (define-key anything-map (kbd "C-p") 'anything-previous-line)
  (define-key anything-map (kbd "C-n") 'anything-next-line)
  (define-key anything-map (kbd "C-v") 'anything-next-source)
  (define-key anything-map (kbd "M-v") 'anything-previous-source))

;; key-chord.el 複数キー同時押しをサポート
;; http://www.emacswiki.org/cgi-bin/wiki/download/key-chord.el
(when (load-p "key-chord")
  (setq key-chord-two-keys-delay 0.02)
  (key-chord-mode 1))

;; Customize minor-mode key priority
(load-p "minor-mode-hack")
;;(lower-minor-mode-map-alist 'ruby-electric-mode)
;;(raise-minor-mode-map-alist 'anthy-minor-mode)

;; 表示設定をロードする
(load-p "faces")

;; 環境設定をロードする (最後に)
(load-p "configs")

;; ~/.emacs.d/site-lisp をすべてロードする (configs のさらに後)
(load-p "loader")

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
