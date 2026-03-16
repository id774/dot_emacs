;;; faces.el --- Configure fonts, colors, and UI faces -*- lexical-binding: t; -*-

;; Author: id774 (More info: http://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 23.4+
;; Extended: Emacs 30+
;; Policy  : Preserve historical behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.

;;; Code:

;; Japanese environment
(set-language-environment 'Japanese)
;; (set-default-coding-systems 'euc-jp-unix)
;; (set-buffer-file-coding-system 'euc-jp-unix)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'euc-jp-unix)
;; (setq file-name-coding-system 'euc-jp-unix)
;; (set-clipboard-coding-system 'iso-2022-jp-unix)
;; (setq default-process-coding-system '(undecided . euc-jp-unix))

;; Prefer UTF-8
(prefer-coding-system 'utf-8-unix)

(if window-system
    (progn
      (cond
       ;; Windows (Meadow3)
       ;; Requires VL Gothic
       ;; http://dicey.org/vlgothic/
       ((eq system-type 'windows-nt)
        (setq default-frame-alist
              (append (list '(font . "VL ゴシック-12")) default-frame-alist)))

       ;; GNU/Linux
       ;; Requires fonts-dejavu and fonts-ipaexfont
       ;; Use DejaVu Sans Mono
       ((eq system-type 'gnu/linux)
        (setq default-frame-alist
              (append (list '(top . 45)
                            '(left . 20)
                            '(width . 150)
                            '(height . 50)
                            '(font . "DejaVu Sans Mono-8"))
                      default-frame-alist))

        ;; Use IPAexGothic for Japanese
        (set-fontset-font t 'japanese-jisx0208
                          (font-spec :family "IPAexGothic"))
        (set-fontset-font t 'japanese-jisx0212
                          (font-spec :family "IPAexGothic"))
        (set-fontset-font t 'katakana-jisx0201
                          (font-spec :family "IPAexGothic"))
        (set-fontset-font t 'han
                          (font-spec :family "IPAexGothic")))

       ;; macOS (use Menlo)
       ((eq system-type 'darwin)
        (setq default-frame-alist
              (append (list '(top . 45)
                            '(left . 20)
                            '(width . 230)
                            '(height . 65)
                            '(font . "Menlo-12"))
                      default-frame-alist))

        ;; Configure Menlo
        (set-face-attribute 'default nil
                            :family "Menlo"
                            :height 120
                            :weight 'normal
                            :slant 'normal)

        ;; Use Hiragino Kaku Gothic ProN W3 for Japanese
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0208
                          '("Hiragino Kaku Gothic ProN W3" . "iso10646-1"))
        (set-fontset-font (frame-parameter nil 'font)
                          'japanese-jisx0212
                          '("Hiragino Kaku Gothic ProN W3" . "iso10646-1"))
        (set-fontset-font (frame-parameter nil 'font)
                          'katakana-jisx0201
                          '("Hiragino Kaku Gothic ProN W3" . "iso10646-1"))

        ;; Disable italics
        (set-face-attribute 'italic nil :slant 'normal)
        (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
        (set-face-attribute 'font-lock-string-face nil :slant 'normal)

        ;; Additional macOS settings
        (setq fixed-width-use-QuickDraw-for-ascii t)
        (setq mac-allow-anti-aliasing t)
        (setq ns-command-modifier 'meta)
        (setq ns-alternate-modifier 'super)
        ))

      ;; Basic frame settings
      (setq default-frame-alist
            (append (list '(foreground-color . "#00FF00")
                          '(background-color . "#000000")
                          '(border-color . "#000000")
                          '(mouse-color . "#00FFFF")
                          '(cursor-color . "#FF0000")
                          '(vertical-scroll-bars . nil))
                    default-frame-alist))

      ;; Highlight the region
      (setq transient-mark-mode t)

      ;; font-lock settings
      ;; Use maximum decoration
      (global-font-lock-mode 1)
      (setq font-lock-support-mode 'jit-lock-mode)
      (setq font-lock-maximum-decoration t)
      ;; Customize default colors
      (add-hook 'font-lock-mode-hook
                '(lambda ()
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

;;; faces.el ends here
