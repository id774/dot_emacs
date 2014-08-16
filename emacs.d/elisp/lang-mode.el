;; ruby-optional-load
(load-p "ruby-optional-load")

;; scala-mode
(require 'scala-mode-auto)

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

;; gtags-mode : global
(when (autoload-p 'gtags-mode "gtags" "GNU GLOBAL" 'interactive)
  (setq gtags-mode-hook
  (function (lambda ()
    (local-set-key "\M-f" 'gtags-find-tag)    ; override etags
    (local-set-key "\M-r" 'gtags-find-rtag)   ; reverse tag
    (local-set-key "\M-s" 'gtags-find-symbol) ; find
    (local-set-key "\C-t" 'gtags-pop-stack)))); pop
  ;; C-mode のときは常に gtags にする
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

(load-p "python-mode-settings")

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

;; pig-mode
(when (load-p "pig-latin-mode")
  (add-to-list 'auto-mode-alist '("\\.pig$" . pig-latin-mode))
  (autoload-p 'pig-latin-mode "pig-latin-mode" "Pig-Latin mode" 'interactive))

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

;; markdown-mode
(when (autoload-p 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" 'interactive)
  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.txt" . markdown-mode) auto-mode-alist)))

;; Erlang
(when (require 'erlang)
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode)))
