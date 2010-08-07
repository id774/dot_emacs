;; configs.el
;; ����Ū�ʴĶ�����

;;�⡼�ɥ饤��˺�����ؿ���ɽ��
;;�ɤ߹��߻��Τߥ������äƤ����Τ�����äȤ����Ƥʤ�
(which-func-mode)

;; blink
(blink-cursor-mode nil)

;; hide tool-bar and scroll-bar
(if window-system
  (progn
    (tool-bar-mode nil)
    (scroll-bar-mode -1)))

;; ��˥塼�С��Ȥ�?
(menu-bar-mode -1)

;; �ۥ�����ޥ����Ȥ�?
(mouse-wheel-mode 1)

;; xterm�Ȥ�gnome-terminal�Ȥ���
(xterm-mouse-mode -1)

;; ;; fringe(������;��Τ褦�˸����Ƥ륢��)
;; (fringe-mode 8)

;; ;; ����ɽ��
(display-time)

;; ���ֹ�����ֹ�
(line-number-mode t)
(column-number-mode t)

;; ����Ÿ��
(auto-image-file-mode)

;; interactive switch buffer
(iswitchb-mode)
(iswitchb-default-keybindings)

;; ��ư�����֥ե������������뤫�ɤ���
(setq auto-save-default nil)

;; �Хå����åץե������������뤫�ɤ���
(setq make-backup-files t)

;; �Хå����åץե��������¸���ֻ���
;; CVS�Ǵ������Ƥ��Ƥ����ꤷ�Ƥ����Ȱ���
;; !path!to!file-name~ ����¸�����
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups")))

;; transient-mark
(setq transient-mark-mode t)

;;isearch �򿧤Ĥ���
(setq search-highlight t)
(setq query-replace-highlight t)
;;(setq isearch-lazy-highlight-initial-delay 0) ; obsolate
(setq lazy-highlight-initial-delay 0)

;; M-x woman
(setq woman-manpath '("/usr/local/man"
		      "/usr/share/man"
		      "/usr/local/share/man"
		      "/sw/man"
		      "/usr/share/man/ja_JP.ujis"))
(setq woman-cache-filename (expand-file-name "~/.emacs.d/woman-cache"))

;; �Хå����åפ��Ȥ���inode���Ѥ��Τ������ʤ�
(setq backup-by-copying t)

;;GC�ֳ�
(setq gc-cons-threshold 1000000)

;; ���ץ�å�����ɽ�� : ��ư��®���ʤ�
(setq inhibit-startup-message t)

;; ����ȿž�����
(setq visible-bell nil)

;; ���ޤ���礭���ե�����Ͽ��դ���Ȼ��֤�����Τǡ���¤����
(setq font-lock-maximum-size nil)

;; ;; fast-lock
;; (setq font-lock-support-mode 'fast-lock-mode)
;; (setq fast-lock-cache-directories '("~/.emacs.d/emacs-flc"))

;; auto-save�ξ��
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")

;; �Ǹ�˲��Ԥ��դ��롣
(setq require-final-newline t)

;; /tmp �Ǥ�ޤ�����������ɡ�
;; (setq temporary-file-directory "~/.emacs.d/tmp")
(setq temporary-file-directory "/dev/shm")

;; 1�Ԥ��ĥ������롣
(setq scroll-conservatively 1)

;;�����Ԥ�������ʤ�
;;emacs21�Ǥϥǥե���ȡ�
(setq next-line-add-newlines nil)

;; 80 ���Ȥ���äȡġ�
(setq fill-column 79)

;; *Messages* ��Ĺ��
(setq message-log-max 200)

;; .gz �ʥե�����Ȥ���Ʃ��Ū�˰���/��ĥ
(auto-compression-mode t)

;; apropos �򤢤���Ȥ��Ȥ�
(setq apropos-do-all t)

;; abbrev
;; (read-abbrev-file "~/.emacs.d/abbrev_defs")
;; (setq save-abbrevs t)

;; version control
(setq vc-follow-symlinks t)
(setq vc-suppress-confirm t)
(setq vc-command-messages t)

;; narrowing�ηٹ���޻�
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; ����������
(setq cursor-in-non-selected-windows nil)

;; ���Զ�Ĵ
(setq-default indicate-empty-lines t)

;; ��Ƭ�� C-k �ǹ����Τ���
(setq kill-whole-line t)

;; �Դ�
;; (setq-default line-spacing 0)

;; Anthy
;; (set-input-method "japanese-anthy")
;; (set-input-method "japanese-prime")

;; C����Ϥ����귲

;; Ruby default style
(c-add-style "ruby"
	     '("bsd"
	       (c-offsets-alist
		(case-label . 2)
		(label . 2)
		(statement-case-intro . 2))))

;; �Ǥ������� stroustrup ��������
(defun-add-hook 'c-mode-common-hook
  (c-set-style "Stroustrup")
  (c-toggle-hungry-state 1)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

;; ��������ԤΥϥ��饤��
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
;; (setq hl-line-face 'underline) ; ����
(hl-line-mode 1)

;; �ե����륪���ץ�ľ����ɤ߼������(C-x j ������)
(add-hook 'find-file-hooks
  (lambda ()
    (cond (view-mode)
      (t
        (view-mode)))
;; �Կ�ɽ��(C-x t ������)
    (cond
      ((>= emacs-major-version '23)
        (linum-mode)))))

;; ����ü���ޤ��֤�
(setq truncate-partial-width-windows nil)

;; fullscreen
;; ��ư���˥�����ɥ�����粽������ϥ����ȥ����Ȥ���
;(set-frame-parameter nil 'fullscreen 'fullboth)
(defun toggle-fullscreen ()
  (interactive)
    (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
      nil
      'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; Redo! (need byte-compile redo.el)
(when (require 'redo nil t)
  (define-key ctl-x-map (if window-system "U" "r") 'redo)
  (define-key global-map [?\C-.] 'redo))

;; Ʃ����
(add-to-list 'default-frame-alist '(alpha . (80 50)))

;; �����Х��������
(load-p "global-set-key")

;; view-mode�����Х��������
(load-p "view-mode-key")

;; key-chord.el���ѥ����Х��������
(load-p "key-chord-define-global")

;; Twitter
(load-p "twitter1-account")
(load-p "twitter2-account")
(load-p "twitter3-account")
(load-p "twitter4-account")
(load-p "twitter5-account")
(load-p "twitter6-account")

;; Twitter�ѥ����Х��������
(load-p "twitter-key")

;; root��ͭ�Υե�����򳫤��Ȥ���tramp��sudo����
(defun file-root-p (filename)
  "Return t if file FILENAME created by root."
  (eq 0 (nth 2 (file-attributes filename))))

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (file-root-p (ad-get-arg 0))
           (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only and owner is root. Open it with sudo? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; �Ķ���ͭ������򤳤���
(if window-system
  (progn
    (cond
      ((eq system-type 'windows-nt)
      )
      ((eq system-type 'gnu/linux)
        (setenv "JAVA_HOME" "/usr/lib/jvm/java-6-sun")
      )
      ((eq system-type 'darwin)
        (setenv "JAVA_HOME" "/System/Library/Frameworks/JavaVM.framework/Versions/1.5.0/Home")
        (cond
          ((< emacs-major-version '23)
            (progn
              (set-frame-parameter nil 'fullscreen 'fullboth) ; ���粽
              ))
          ((>= emacs-major-version '23)
            (progn
              (tool-bar-mode 0) ; toolbar��ɽ��
              ))
        )
      )
    )
  )
)

;; ����������
(load-p "local")

;; Local Variables:
;; mode : emacs-lisp
;; coding : euc-jp-unix
;; End:
