;; Unix default settings

;; ���ܸ�����
(set-language-environment 'Japanese)
;; �Ƕ�⤦$LANG�Ǥ�����Ȥ������ˤʤä���
;; (set-default-coding-systems 'euc-jp-unix)
;; (set-buffer-file-coding-system 'euc-jp-unix)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'euc-jp-unix)
;; (setq file-name-coding-system 'euc-jp-unix)
;; (set-clipboard-coding-system 'iso-2022-jp-unix)
;; (setq default-process-coding-system '(undecided . euc-jp-unix))

;; UTF-8��ͥ���̤�⤯����
(prefer-coding-system 'utf-8-unix)

(if window-system
  (progn
    (cond
;; Windows(Meadow3)��GUI����
;; VL�����å��ե����ɬ��
;; http://dicey.org/vlgothic/
      ((eq system-type 'windows-nt)
        (w32-add-font
         "vl-gothic-12"
         '((spec
            ((:char-spec ascii :height any)
             strict
             (w32-logfont "VL �����å�" 0 -12 400 0 nil nil nil 128 1 3 49))
             ((:char-spec ascii :height any :weight bold)
              strict
              (w32-logfont "VL �����å�" 0 -12 700 0 nil nil nil 128 1 3 49))
             ((:char-spec ascii :height any :slant italic)
             strict
             (w32-logfont "VL �����å�" 0 -12 400 0 t nil nil 128 1 3 49))
             ((:char-spec ascii :height any :weight bold :slant italic)
              strict
              (w32-logfont "VL �����å�" 0 -12 700 0 t nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any)
              strict
              (w32-logfont "VL �����å�" 0 -12 400 0 nil nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any :weight bold)
              strict
              (w32-logfont "VL �����å�" 0 -12 700 0 nil nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any :slant italic)
             strict
             (w32-logfont "VL �����å�" 0 -12 400 0 t nil nil 128 1 3 49))
             ((:char-spec japanese-jisx0208 :height any :weight bold :slant italic)
              strict
              (w32-logfont "VL �����å�" 0 -12 700 0 t nil nil 128 1 3 49)
              ((spacing . -1))
             ))))
        (setq default-frame-alist
              (append (list '(top . 0) ; ��ư����ɽ�����֡ʾ夫���
                            '(left . 0) ; ��ư����ɽ�����֡ʺ������
                            '(width . 120) ; ��ư���Υ�����������
                            '(height . 40) ; ��ư���Υ������ʽġ�
                            '(font . "vl-gothic-12"); VL Gothic
                            ))))
;; GNU/Linux��GUI����
;; Bitstream Vera Sans Mono/VL�����å������
;; (��:ttf-bitstream-vera�ѥå�����)
      ((eq system-type 'gnu/linux)
        (setq default-frame-alist ; ThinkPad X60/X61 �˺�Ŭ��
              (append (list '(top . 0) ; ��ư����ɽ�����֡ʾ夫���
                            '(left . 0) ; ��ư����ɽ�����֡ʺ������
                            '(width . 120) ; ��ư���Υ�����������
                            '(height . 40) ; ��ư���Υ������ʽġ�
                            )))
        (set-default-font "Bitstream Vera Sans Mono-8")
        ;;(set-default-font "DejaVu Sans Mono-8") ;; Ubuntu Lucid
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("VL �����å�" . "unicode-bmp"))
      )
      ((eq system-type 'darwin)
;; Mac OS X Cocoa/Carbon Emacs��GUI����
;; http://diary.mrmt.net/item/1356
        (cond
          ((< emacs-major-version '23)
            (progn
              (setq default-frame-alist
                    (append (list '(top . 0) ; ��ư����ɽ�����֡ʾ夫���
                                  '(left . 0) ; ��ư����ɽ�����֡ʺ������
                                  '(width . 180) ; ��ư���Υ�����������
                                  '(height . 45) ; ��ư���Υ������ʽġ�
                                  )))
              ))
          ((>= emacs-major-version '23)
            (progn
              (setq default-frame-alist ; 13inch MacBook Pro �˺�Ŭ��
                    (append (list '(top . 0) ; ��ư����ɽ�����֡ʾ夫���
                                  '(left . 0) ; ��ư����ɽ�����֡ʺ������
                                  '(width . 210) ; ��ư���Υ�����������
                                  '(height . 60) ; ��ư���Υ������ʽġ�
                                  )))
              ;; (set-input-method "MacOSX")
              (setq ns-command-modifier (quote meta))
              (setq ns-alternate-modifier (quote super))
              (setq my-font "-*-*-medium-r-normal--10-*-*-*-*-*-fontset-hiramaru")
              (setq fixed-width-use-QuickDraw-for-ascii t)
              (setq mac-allow-anti-aliasing t)
              (set-default-font my-font)
              (add-to-list 'default-frame-alist `(font . ,my-font))
              (set-fontset-font
                (frame-parameter nil 'font)
                'japanese-jisx0208
                '("Hiragino Maru Gothic Pro" . "iso10646-1"))
              (setq face-font-rescale-alist
                '(("^-apple-hiragino.*" . 1.2)
                (".*osaka-bold.*" . 1.2)
                (".*osaka-medium.*" . 1.2)
                (".*courier-bold-.*-mac-roman" . 1.0)
                (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                (".*monaco-bold-.*-mac-roman" . 0.9)
                ("-cdac$" . 1.3)))))))
      )
;; �ե졼������
    (setq default-frame-alist
          (append (list '(foreground-color . "#00FF00") ; ʸ���ο�
                        '(background-color . "#000000") ; �طʤο�
                        '(border-color . "#000000") ;
                        '(mouse-color . "#00FFFF") ;
                        '(cursor-color . "#FF0000") ; ��������ο�
                        '(vertical-scroll-bars . nil) ;
                   )
                  default-frame-alist))
;;�꡼�����˿����դ���
    (setq transient-mark-mode t)
;;�ե���ȥ�å�
    (global-font-lock-mode 1)
    (setq font-lock-support-mode 'jit-lock-mode)
;; ���Ť��Ϻ���¤�
    (setq font-lock-maximum-decoration t)
;; �ǥե���Ȥο��Ť����Ѥ���
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
