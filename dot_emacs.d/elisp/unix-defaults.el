;; Unix default settings

;; 日本語設定
(set-language-environment 'Japanese)
;; 最近もう$LANGでいいやという気になった。
;; (set-default-coding-systems 'euc-jp-unix)
;; (set-buffer-file-coding-system 'euc-jp-unix)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'euc-jp-unix)
;; (setq file-name-coding-system 'euc-jp-unix)
;; (set-clipboard-coding-system 'iso-2022-jp-unix)
;; (setq default-process-coding-system '(undecided . euc-jp-unix))

;; UTF-8の優先順位を高くする
(prefer-coding-system 'utf-8-unix)

(if window-system
  (progn
    (cond
;; Windows(Meadow3)用GUI設定
;; VLゴシックフォント必要
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
              (append (list '(top . 0) ; 起動時の表示位置（上から）
                            '(left . 0) ; 起動時の表示位置（左から）
                            '(width . 120) ; 起動時のサイズ（幅）
                            '(height . 40) ; 起動時のサイズ（縦）
                            '(font . "vl-gothic-12"); VL Gothic
                            ))))
;; GNU/Linux用GUI設定
;; Bitstream Vera Sans Mono/VLゴシックを指定
;; (要:ttf-bitstream-veraパッケージ)
      ((eq system-type 'gnu/linux)
        (setq default-frame-alist ; ThinkPad X60/X61 に最適化
              (append (list '(top . 0) ; 起動時の表示位置（上から）
                            '(left . 0) ; 起動時の表示位置（左から）
                            '(width . 120) ; 起動時のサイズ（幅）
                            '(height . 40) ; 起動時のサイズ（縦）
                            )))
        (set-default-font "Bitstream Vera Sans Mono-8")
        ;;(set-default-font "DejaVu Sans Mono-8") ;; Ubuntu Lucid
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("VL ゴシック" . "unicode-bmp"))
      )
      ((eq system-type 'darwin)
;; Mac OS X Cocoa/Carbon Emacs用GUI設定
;; http://diary.mrmt.net/item/1356
        (cond
          ((< emacs-major-version '23)
            (progn
              (setq default-frame-alist
                    (append (list '(top . 0) ; 起動時の表示位置（上から）
                                  '(left . 0) ; 起動時の表示位置（左から）
                                  '(width . 180) ; 起動時のサイズ（幅）
                                  '(height . 45) ; 起動時のサイズ（縦）
                                  )))
              ))
          ((>= emacs-major-version '23)
            (progn
              ;; (set-input-method "MacOSX")
              (setq ns-command-modifier (quote meta))
              (setq ns-alternate-modifier (quote super))
              (if window-system (progn
                 (create-fontset-from-fontset-spec
                 (concat
                  "-*-fixed-medium-r-normal-*-14-*-*-*-*-*-fontset-osaka16,"
                  "japanese-jisx0208:-apple-osaka-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
                  "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
                 (set-default-font "fontset-osaka16")
                 (setq default-frame-alist 
                   (append 
                     '((font . "fontset-osaka16"))
                       (list '(top . 0) ; 起動時の表示位置（上から）
                             '(left . 0) ; 起動時の表示位置（左から）
                             '(width . 210) ; 起動時のサイズ（幅）
                             '(height . 60) ; 起動時のサイズ（縦）
                 )))
              ))
          ))))
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
;;

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
