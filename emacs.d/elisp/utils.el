;; utils.el
;; .emacs ����ǻȤ��ؿ�

;; http://www.sodan.org/~knagano/emacs/dotemacs.html
;; �Ȥ����ȡ�

(defun autoload-p (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))
(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook �Υ����ꥢ����������ؿ��˥ѥå����� hook ���ɲä��롣"
  `(add-hook ,hookname
	     (function (lambda () ,@sexplist))))
(defun load-p (loadlib)
  "������ load���ɤ߹��ߤ˼��Ԥ��Ƥ⤽���ǻߤޤ�ʤ���"
  ;; missing-ok ���ɤ�Ǥߤơ�����ʤ餳�ä��� message �Ǥ�Ф��Ƥ���
  (let ((load-status (load loadlib t)))
    (or load-status
	(message (format "failed to load %s" loadlib)))
    load-status))


;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
