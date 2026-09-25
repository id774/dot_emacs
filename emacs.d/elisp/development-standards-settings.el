;;; development-standards-settings.el --- Integrate project-provided team development standards -*- lexical-binding: t; -*-

;; Author: id774 (More info: https://id774.net)
;; Source Code: https://github.com/id774/dot_emacs
;; License: The GPL version 3, or LGPL version 3 (Dual License).
;; Contact: idnanashi@gmail.com

;; Support : Emacs 30+
;; Policy  : Preserve supported behavior and maintain backward compatibility.
;; Package : DOT_EMACS

;;; Commentary:
;; Part of the DOT_EMACS configuration.
;; See doc/GUIDELINES for compatibility and maintenance policy.
;;
;; Use the development standards a project provides, through EditorConfig,
;; Prettier and ESLint, with the same effect as the editor extensions a team
;; may require.  The project configuration is the source of truth: DOT_EMACS
;; defines no formatting, lint or editor rule of its own, and an integration
;; stays inactive when the project provides no applicable configuration.

;;; Code:

(require 'subr-x)
(require 'flymake)
(require 'editorconfig)

;; EditorConfig: the built-in support finds and applies .editorconfig files
(editorconfig-mode 1)

(defvar-local development-standards--prettier-executable nil
  "Prettier executable used to format this buffer, or nil when inactive.")

(defvar-local development-standards--eslint-executable nil
  "ESLint executable used to lint this buffer, or nil when inactive.")

(defvar-local development-standards--eslint-process nil
  "Current ESLint process for this buffer's Flymake check.")

(defun development-standards--find-executable (tool)
  "Return the executable for TOOL applicable to the current file, or nil.
The nearest node_modules/.bin/TOOL above the file comes first; `PATH'
is searched only when the project provides no local executable."
  (when buffer-file-name
    (let* ((local (concat "node_modules/.bin/" tool))
           (root (locate-dominating-file
                  buffer-file-name
                  (lambda (dir)
                    (file-executable-p (expand-file-name local dir))))))
      (if root
          (expand-file-name local root)
        (executable-find tool)))))

(defun development-standards--call (program &rest args)
  "Run PROGRAM with ARGS and return (STATUS . STDOUT).  Discard stderr."
  (let ((coding-system-for-read 'utf-8))
    (with-temp-buffer
      (let ((status (apply #'call-process program nil '(t nil) nil args)))
        (cons status (buffer-string))))))

(defun development-standards--parse-json (string)
  "Parse the JSON STRING, or return nil when it is not valid JSON."
  (condition-case nil
      (json-parse-string string :null-object nil :false-object nil)
    (json-error nil)))

(defun development-standards--prettier-applicable-p (prettier)
  "Return non-nil when PRETTIER has a configuration for the current file.
The file must also be supported by Prettier and not ignored by it."
  (let ((config (development-standards--call
                 prettier "--find-config-path" buffer-file-name)))
    (when (and (eq (car config) 0)
               (not (string-empty-p (string-trim (cdr config)))))
      (let* ((info (development-standards--call
                    prettier "--file-info" buffer-file-name))
             (json (and (eq (car info) 0)
                        (development-standards--parse-json (cdr info))))
             (parser (and (hash-table-p json)
                          (gethash "inferredParser" json))))
        (and (hash-table-p json)
             (not (gethash "ignored" json))
             (stringp parser)
             (not (string-empty-p parser)))))))

(defun development-standards-prettier-format-buffer ()
  "Format the current buffer with the project's Prettier configuration.
A Prettier failure leaves the buffer unchanged and shows a warning."
  (interactive)
  (unless development-standards--prettier-executable
    (user-error "Prettier integration is not active in this buffer"))
  (let ((output (generate-new-buffer " *development-standards-prettier*"))
        (stderr (make-temp-file "development-standards-prettier")))
    (unwind-protect
        (condition-case err
            (let* ((coding-system-for-read 'utf-8)
                   (coding-system-for-write 'utf-8)
                   (status (save-restriction
                             (widen)
                             (call-process-region
                              (point-min) (point-max)
                              development-standards--prettier-executable
                              nil (list output stderr) nil
                              "--stdin-filepath" buffer-file-name))))
              (if (eq status 0)
                  (save-restriction
                    (widen)
                    (replace-buffer-contents output))
                (display-warning
                 'development-standards
                 (format "Prettier failed (%s) for %s; the buffer is unchanged:\n%s"
                         status buffer-file-name
                         (with-temp-buffer
                           (insert-file-contents stderr)
                           (string-trim (buffer-string)))))))
          (error
           (display-warning
            'development-standards
            (format "Prettier failed for %s; the buffer is unchanged: %s"
                    buffer-file-name (error-message-string err)))))
      (kill-buffer output)
      (delete-file stderr))))

(defun development-standards--eslint-configured-p (eslint)
  "Return non-nil when ESLINT resolves a configuration for the current file."
  (let ((config (development-standards--call
                 eslint "--print-config" buffer-file-name)))
    (and (eq (car config) 0)
         (development-standards--parse-json (cdr config)))))

(defun development-standards--eslint-position (line column)
  "Return the position of 1-based LINE and COLUMN, or nil when invalid."
  (when (and (natnump line) (natnump column) (> line 0) (> column 0))
    (save-excursion
      (goto-char (point-min))
      (when (zerop (forward-line (1- line)))
        (let ((pos (+ (point) (1- column))))
          (when (<= pos (line-end-position))
            pos))))))

(defun development-standards--eslint-diagnostic (source message)
  "Return a Flymake diagnostic in SOURCE for the ESLint MESSAGE."
  (with-current-buffer source
    (save-restriction
      (widen)
      (let* ((line (gethash "line" message))
             (column (gethash "column" message))
             (start (development-standards--eslint-position line column))
             (end (development-standards--eslint-position
                   (gethash "endLine" message) (gethash "endColumn" message)))
             (region (if start
                         (cons start
                               (if (and end (> end start))
                                   end
                                 (cdr (flymake-diag-region source line column))))
                       (flymake-diag-region
                        source (if (natnump line) line 1))))
             (severity (gethash "severity" message))
             (rule (gethash "ruleId" message))
             (text (gethash "message" message)))
        (flymake-make-diagnostic
         source (car region) (cdr region)
         (cond ((eql severity 2) :error)
               ((eql severity 1) :warning)
               (t :note))
         (if rule (format "%s [%s]" text rule) text))))))

(defun development-standards--eslint-report (source proc report-fn)
  "Report the result of the finished ESLint PROC for SOURCE to REPORT-FN."
  (let ((status (process-exit-status proc))
        (results (and (eq (process-status proc) 'exit)
                      (memq (process-exit-status proc) '(0 1))
                      (with-current-buffer (process-buffer proc)
                        (development-standards--parse-json (buffer-string))))))
    (if (vectorp results)
        (funcall report-fn
                 (mapcan (lambda (result)
                           (mapcar (lambda (message)
                                     (development-standards--eslint-diagnostic
                                      source message))
                                   (gethash "messages" result)))
                         results))
      (funcall report-fn :panic
               :explanation
               (format "ESLint failed (%s %s): %s"
                       (process-status proc) status
                       (with-current-buffer (process-get proc 'stderr)
                         (string-trim (buffer-string))))))))

(defun development-standards-eslint-flymake (report-fn &rest _args)
  "Flymake backend that reports the project's ESLint diagnostics to REPORT-FN."
  (when (process-live-p development-standards--eslint-process)
    (kill-process development-standards--eslint-process))
  (let* ((source (current-buffer))
         (stderr (generate-new-buffer " *development-standards-eslint-stderr*"))
         (proc
          (make-process
           :name "development-standards-eslint"
           :buffer (generate-new-buffer " *development-standards-eslint*")
           :stderr stderr
           :command (list development-standards--eslint-executable
                          "--stdin" "--stdin-filename" buffer-file-name
                          "--format" "json")
           :coding 'utf-8
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (unwind-protect
                   (when (and (buffer-live-p source)
                              (eq proc (buffer-local-value
                                        'development-standards--eslint-process
                                        source)))
                     (development-standards--eslint-report
                      source proc report-fn))
                 (kill-buffer (process-buffer proc))
                 (kill-buffer stderr)))))))
    (process-put proc 'stderr stderr)
    (setq development-standards--eslint-process proc)
    (save-restriction
      (widen)
      (process-send-region proc (point-min) (point-max)))
    (process-send-eof proc)))

(defun development-standards-setup ()
  "Enable the development standards the visited file's project provides."
  (when (and buffer-file-name
             (not (file-remote-p buffer-file-name)))
    (let ((prettier (development-standards--find-executable "prettier")))
      (when (and prettier
                 (development-standards--prettier-applicable-p prettier))
        (setq development-standards--prettier-executable prettier)
        (add-hook 'before-save-hook
                  #'development-standards-prettier-format-buffer nil t)))
    (let ((eslint (development-standards--find-executable "eslint")))
      (when (and eslint
                 (development-standards--eslint-configured-p eslint))
        (setq development-standards--eslint-executable eslint)
        (add-hook 'flymake-diagnostic-functions
                  #'development-standards-eslint-flymake nil t)
        (flymake-mode 1)))))

(add-hook 'find-file-hook 'development-standards-setup)

;;; development-standards-settings.el ends here
