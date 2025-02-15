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
        (setq default-frame-alist
              (append (list '(font . "VL ゴシック-12")) default-frame-alist)))
       ;; GNU/Linux 用設定
       ;; Bitstream Vera Sans Mono/VL ゴシックを指定
       ;; (要 :ttf-bitstream-vera パッケージ)
       ((eq system-type 'gnu/linux)
        (setq default-frame-alist
              (append (list '(font . "Bitstream Vera Sans Mono-8")) default-frame-alist)))
       ;; macOS 用設定（Menlo フォントを使用）
       ((eq system-type 'darwin)
        (setq default-frame-alist
              (append (list '(top . 45)
                            '(left . 20)
                            '(width . 230)
                            '(height . 65)
                            '(font . "Menlo-12"))
                      default-frame-alist))

        ;; Menlo を設定
        (set-face-attribute 'default nil
                            :family "Menlo"
                            :height 120
                            :weight 'normal
                            :slant 'normal)

        ;; 日本語フォントを Hiragino Kaku Gothic ProN W3 に設定
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("Hiragino Kaku Gothic ProN W3" . "iso10646-1"))
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0212
                          '("Hiragino Kaku Gothic ProN W3" . "iso10646-1"))
        (set-fontset-font (frame-parameter nil 'font)
                          'katakana-jisx0201
                          '("Hiragino Kaku Gothic ProN W3" . "iso10646-1"))

        ;; 斜体の無効化
        (set-face-attribute 'italic nil :slant 'normal)
        (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
        (set-face-attribute 'font-lock-string-face nil :slant 'normal)

        ;; Mac用の追加設定
        (setq fixed-width-use-QuickDraw-for-ascii t)
        (setq mac-allow-anti-aliasing t)
        (setq ns-command-modifier 'meta)
        (setq ns-alternate-modifier 'super)
       ))

      ;; フレームの基本設定
      (setq default-frame-alist
            (append (list '(foreground-color . "#00FF00")
                          '(background-color . "#000000")
                          '(border-color . "#000000")
                          '(mouse-color . "#00FFFF")
                          '(cursor-color . "#FF0000")
                          '(vertical-scroll-bars . nil))
                    default-frame-alist))

      ;; リージョンに色を付ける
      (setq transient-mark-mode t)

      ;; フォントロックの設定
      ;; 色づけは最大限に
      (global-font-lock-mode 1)
      (setq font-lock-support-mode 'jit-lock-mode)
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
                                        (set-face-bold-p 'font-lock-warning-face nil)))))
