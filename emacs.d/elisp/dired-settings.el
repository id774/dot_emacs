
;; dired を 2 つのウィンドウで開いている時に
;; デフォルトの移動 or コピー先をもう一方の dired で開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
;; dired バッファで C-s した時にファイル名だけにマッチするように
(setq dired-isearch-filenames t)

;; dired-find-alternate-file の有効化
(put 'dired-find-alternate-file 'disabled nil)

;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "a") 'dired-find-file)

;; BS/DEL でディレクトリを上に
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
(define-key dired-mode-map (kbd "<DEL>") 'dired-up-directory)

;; カーソルキーでウィンドウの移動
(define-key dired-mode-map (kbd "<left>")  'windmove-left)
(define-key dired-mode-map (kbd "<right>") 'windmove-right)
(define-key dired-mode-map (kbd "<down>")  'windmove-down)
(define-key dired-mode-map (kbd "<up>")    'windmove-up)

