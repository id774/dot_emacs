;; Enable asynchronous byte compilation for Emacs Lisp buffers
(when (load-p "auto-async-byte-compile")
  ;; Skip files located under /junk/
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))
