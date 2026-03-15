;; zlc
;; Configure zlc (Zsh-like completion) for minibuffer completion.

(when (require 'zlc)
  ;; Enable zlc completion mode
  (zlc-mode t)

  ;; Do not select completion immediately when only one candidate exists
  (setq zlc-select-completion-immediately nil)

  ;; Configure navigation keys in minibuffer completion
  (let ((map minibuffer-local-map))
    (define-key map (kbd "<down>")  'zlc-select-next-vertical)
    (define-key map (kbd "<up>")    'zlc-select-previous-vertical)
    (define-key map (kbd "<right>") 'zlc-select-next)
    (define-key map (kbd "<left>")  'zlc-select-previous)))
