;; Faces

;; 日本語設定
(set-language-environment 'Japanese)
;; (set-default-coding-systems 'euc-jp-unix)
;; (set-buffer-file-coding-system 'euc-jp-unix)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'euc-jp-unix)
;; (setq file-name-coding-system 'euc-jp-unix)
;; (set-clipboard-coding-system 'iso-2022-jp-unix)
;; (setq default-process-coding-system '(undecided . euc-jp-unix))

;; UTF-8 の優先順位を高くする
(prefer-coding-system 'utf-8-unix)

(if window-system
  (progn
    (cond
;; Windows(Meadow3) 用設定
;; VL ゴシックフォント必要
;; http://dicey.org/vlgothic/
      ((eq system-type 'windows-nt)
        (w32-add-font
         "vl-gothic-12"
         '((spec
            ((:char-spec ascii :height any)
             strict
             (w32-logfont "VL ゴシック" 0 -12 400 0 nil nil nil 128 1 3 49))
             ((:char-spec ascii :height any :weight bold)
              strict
              (w32-logfont "VL ゴシック" 0 -12 700 0 nil nil nil 128 1 3 49))
             ((:char-spec ascii :height any :slant italic)
             strict
             (w32-logfont "VL ゴシック" 0 -12 400 0 t nil nil 128 1 3 49))
             ((:char-spec ascii :height any :weight bold :slant italic)
              strict
              (w32-logfont "VL ゴシック" 0 -12 700 0 t nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any)
              strict
              (w32-logfont "VL ゴシック" 0 -12 400 0 nil nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any :weight bold)
              strict
              (w32-logfont "VL ゴシック" 0 -12 700 0 nil nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any :slant italic)
             strict
             (w32-logfont "VL ゴシック" 0 -12 400 0 t nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
              strict
              (w32-logfont "VL ゴシック" 0 -12 700 0 t nil nil 128 1 3 49)
              ((spacing . -1))
             ))))
        (setq default-frame-alist
              (append (list '(top . 10) ; 起動時の表示位置（上から）
                            '(left . 10) ; 起動時の表示位置（左から）
                            '(width . 120) ; 起動時のサイズ（幅）
                            '(height . 40) ; 起動時のサイズ（縦）
                            '(font . "vl-gothic-12"); VL Gothic
                            ))))
;; GNU/Linux 用設定
;; Bitstream Vera Sans Mono/VL ゴシックを指定
;; (要 :ttf-bitstream-vera パッケージ)
      ((eq system-type 'gnu/linux)
        (setq default-frame-alist ; ThinkPad X60/X61 に最適化
              (append (list '(top . 10) ; 起動時の表示位置（上から）
                            '(left . 10) ; 起動時の表示位置（左から）
                            '(width . 120) ; 起動時のサイズ（幅）
                            '(height . 40) ; 起動時のサイズ（縦）
                            )))
        (set-default-font "Bitstream Vera Sans Mono-8")
        ;; (set-default-font "DejaVu Sans Mono-8") ;; Ubuntu Lucid
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("VL ゴシック" . "unicode-bmp"))
      )
      ((eq system-type 'darwin)
;; Mac OS X Cocoa/Carbon Emacs 用設定
;; http://diary.mrmt.net/item/1356
        (cond
          ((< emacs-major-version '23)
            (progn
              (setq default-frame-alist
                    (append (list '(top . 10) ; 起動時の表示位置（上から）
                                  '(left . 10) ; 起動時の表示位置（左から）
                                  '(width . 180) ; 起動時のサイズ（幅）
                                  '(height . 45) ; 起動時のサイズ（縦）
                                  )))
              ))
          ((>= emacs-major-version '23)
            (progn
              (setq default-frame-alist ; 13inch MacBook Pro に最適化
                    (append (list '(top . 10) ; 起動時の表示位置（上から）
                                  '(left . 10) ; 起動時の表示位置（左から）
                                  '(width . 180) ; 起動時のサイズ（幅）
                                  '(height . 50) ; 起動時のサイズ（縦）
                                  )))
              ;; (set-input-method "MacOSX")
              (setq fixed-width-use-QuickDraw-for-ascii t)
              (setq mac-allow-anti-aliasing t)
              (set-face-attribute 'default nil
                                  :family "monaco"
                                  :height 120) ; 相対的な文字の大きさを決定する
              (set-fontset-font
               (frame-parameter nil 'font)
               'japanese-jisx0208
               '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
              (set-fontset-font
               (frame-parameter nil 'font)
               'japanese-jisx0212
               '("Hiragino Kaku Gothic Pro" . "iso10646-1"))
              ;;; Unicode フォント
              (set-fontset-font
               (frame-parameter nil 'font)
               'mule-unicode-0100-24ff
               '("monaco" . "iso10646-1"))
             ;;; キリル，ギリシア文字設定
             ;;; 注意： この設定だけでは古代ギリシア文字、コプト文字は表示できない
             ;;; http://socrates.berkeley.edu/~pinax/greekkeys/NAUdownload.html が必要
             ;;; キリル文字
              (set-fontset-font
               (frame-parameter nil 'font)
               'cyrillic-iso8859-5
               '("monaco" . "iso10646-1"))
             ;;; ギリシア文字
              (set-fontset-font
               (frame-parameter nil 'font)
               'greek-iso8859-7
               '("monaco" . "iso10646-1"))
              (setq face-font-rescale-alist
                    '(("^-apple-hiragino.*" . 1.2)
                      (".*osaka-bold.*" . 1.2)
                      (".*osaka-medium.*" . 1.2)
                      (".*courier-bold-.*-mac-roman" . 1.0)
                      (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                      (".*monaco-bold-.*-mac-roman" . 0.9)
                      ("-cdac$" . 1.3)))
              (setq ns-command-modifier (quote meta))
              (setq ns-alternate-modifier (quote super))
          ))
        ))
      )
;; フレーム設定
    (setq default-frame-alist
          (append (list '(foreground-color . "#00FF00") ; 文字の色
                        '(background-color . "#000000") ; 背景の色
                        '(border-color . "#000000") ;
                        '(mouse-color . "#00FFFF") ;
                        '(cursor-color . "#FF0000") ; カーソルの色
                        '(vertical-scroll-bars . nil) ;
                   )
                  default-frame-alist))
;;リージョンに色を付ける
    (setq transient-mark-mode t)
;;フォントロック
    (global-font-lock-mode 1)
    (setq font-lock-support-mode 'jit-lock-mode)
;; 色づけは最大限に
    (setq font-lock-maximum-decoration t)
;; デフォルトの色づけを変える
    (add-hook 'font-lock-mode-hook '(lambda ()
      (set-face-foreground 'font-lock-builtin-face "spring green")
      (set-face-foreground 'font-lock-comment-face "slate gray")
      (set-face-foreground 'font-lock-string-face  "spring green")
      (set-face-foreground 'font-lock-keyword-face "khaki")
      (set-face-foreground 'font-lock-constant-face "violet")
      (set-face-foreground 'font-lock-function-name-face "hot pink")
      (set-face-foreground 'font-lock-variable-name-face "hot pink")
      (set-face-foreground 'font-lock-type-face "cyan")
      (set-face-foreground 'font-lock-warning-face "magenta")
      (set-face-bold-p 'font-lock-function-name-face t)
      (set-face-bold-p 'font-lock-warning-face nil)
    ))
))

