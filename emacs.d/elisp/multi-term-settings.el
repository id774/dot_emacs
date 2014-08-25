(eval-when-compile (require 'cl))
(require 'multi-term)

(setq term-default-fg-color "Green"
      term-default-bg-color "Black")

(lexical-let
    ((emacs24
      [term term-color-black
            term-color-red
            term-color-green
            term-color-yellow
            term-color-blue
            term-color-magenta
            term-color-cyan
            term-color-white])
     (emacs23
      [unspecified
       "black" "red3" "green3" "yellow3"
       "DeepSkyBlue" ; directory
       "magenta1" "cyan3" "white"]))
  (setq ansi-term-color-vector
        (if (version< "24.0.0" emacs-version)
            emacs24
          emacs23)))

;; my-keybinds for keybinds -e
(defun term-send-forward-char ()
  (interactive)
  (term-send-raw-string "\C-f"))

(defun term-send-backward-char ()
  (interactive)
  (term-send-raw-string "\C-b"))

(defun term-send-previous-line ()
  (interactive)
  (term-send-raw-string "\C-p"))

(defun term-send-next-line ()
  (interactive)
  (term-send-raw-string "\C-n"))

(add-hook 'term-mode-hook
          '(lambda ()
             (let* ((key-and-func
                     `(("\C-p"           term-send-previous-line)
                       ("\C-n"           term-send-next-line)
                       ("\C-b"           term-send-backward-char)
                       ("\C-f"           term-send-forward-char)
                       (,(kbd "C-h")     term-send-backspace)
                       (,(kbd "C-y")     term-paste)
                       (,(kbd "ESC ESC") term-send-raw)
                       (,(kbd "C-S-p")   multi-term-prev)
                       (,(kbd "C-S-n")   multi-term-next)
                       (,(kbd "<up>")    windmove-up)
                       (,(kbd "<down>")  windmove-down)
                       (,(kbd "<left>")  windmove-left)
                       (,(kbd "<right>") windmove-right)
                       ;; ("\C-r"           helm-shell-history)
                       )))
               (loop for (keybind function) in key-and-func do
                     (define-key term-raw-map keybind function)))))

