;;¸�ߤ��ʤ��ե�����򳫤����Ȥ����Ȥ�ǰ����
;;fj.editor.emacs���
(add-hook 'find-file-not-found-hooks 'new-file-p)
(defun new-file-p ()
  (interactive)
  (or (y-or-n-p
       (format "\"%s\"not found. Create this file?"
	       buffer-file-name))))

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
