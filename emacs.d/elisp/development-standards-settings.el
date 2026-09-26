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
;; Make the development capabilities a team development standard requires,
;; such as EditorConfig, Prettier, ESLint, SQL Formatter and Prisma, available
;; from Emacs.  Reproducing another editor's extensions, user interface,
;; command palette, operating procedure, on-save execution, background
;; execution, continuous diagnostics, settings screens or other workflows is
;; not a goal.  A capability may be provided through an Emacs-native explicit
;; command or key binding, and automatic behavior is used only where the
;; development standard itself or existing DOT_EMACS behavior requires it.
;;
;; Project-provided configuration and project-local tools are the source of
;; truth wherever they exist.  DOT_EMACS invents no project formatting, lint
;; or schema rule of its own.  A command that the user runs explicitly to
;; invoke an external tool may rely on the default behavior that tool itself
;; defines.  The absence of an optional external tool is never a startup
;; failure.  No priority, fallback, mutual exclusion or other coordination
;; rule is added between unrelated existing integrations.  This module is a
;; version-gated enhancement for GNU Emacs 30+ and does not change the
;; behavior of GNU Emacs 23.4 through 29.x.
;;
;; This module is intentionally loaded from source and excluded from byte
;; compilation.  Its functionality is available only on GNU Emacs 30 and
;; newer, while DOT_EMACS itself and its installer continue to support GNU
;; Emacs 23.4 and later.  Adding this file to the installer's ordinary byte
;; compilation targets would therefore make older supported Emacs versions
;; attempt to compile code that is not intended for them.
;;
;; The installer could add a separate Emacs-30-only compilation branch, but
;; this module mainly registers hooks, configures buffer-local integration,
;; and invokes external development tools.  The expected benefit from byte
;; compilation does not justify adding version-specific compilation control
;; flow to the installer solely for this module.  It is therefore source-loaded
;; only on Emacs 30+ and is also excluded from automatic asynchronous byte
;; compilation so that the source-load decision remains consistent.

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

(defun development-standards--check-local-file (regexp kind)
  "Signal a user error unless the buffer visits a local KIND file.
The file name must match REGEXP."
  (unless (and buffer-file-name
               (not (file-remote-p buffer-file-name))
               (string-match-p regexp buffer-file-name))
    (user-error "Not visiting a local %s file" kind)))

(defun development-standards--require-executable (tool)
  "Return the executable for TOOL, or signal a user error when none exists."
  (or (development-standards--find-executable tool)
      (user-error "%s executable not found" tool)))

(defun development-standards-sql-format-buffer ()
  "Format the current SQL buffer with SQL Formatter.
SQL Formatter resolves its own configuration and defaults.  A failure
leaves the buffer unchanged and shows a warning.  The buffer is not saved."
  (interactive)
  (development-standards--check-local-file "\\.\\(sql\\|q\\)\\'" "SQL")
  (let ((sql-formatter
         (development-standards--require-executable "sql-formatter"))
        (default-directory (file-name-directory buffer-file-name))
        (output (generate-new-buffer " *development-standards-sql-formatter*"))
        (stderr (make-temp-file "development-standards-sql-formatter")))
    (unwind-protect
        (condition-case err
            (let* ((coding-system-for-read 'utf-8)
                   (coding-system-for-write 'utf-8)
                   (status (save-restriction
                             (widen)
                             (call-process-region
                              (point-min) (point-max) sql-formatter
                              nil (list output stderr) nil))))
              (if (eq status 0)
                  (save-restriction
                    (widen)
                    (replace-buffer-contents output))
                (display-warning
                 'development-standards
                 (format "SQL Formatter failed (%s) for %s; the buffer is unchanged:\n%s"
                         status buffer-file-name
                         (with-temp-buffer
                           (insert-file-contents stderr)
                           (string-trim (buffer-string)))))))
          (error
           (display-warning
            'development-standards
            (format "SQL Formatter failed for %s; the buffer is unchanged: %s"
                    buffer-file-name (error-message-string err)))))
      (kill-buffer output)
      (delete-file stderr))))

(defun development-standards--prisma-run (command unsaved-message)
  "Run Prisma COMMAND on the current schema file.
Signal a user error with UNSAVED-MESSAGE when the buffer is modified.
Return non-nil on success; on failure show a warning and return nil."
  (development-standards--check-local-file "\\.prisma\\'" "Prisma")
  (when (buffer-modified-p)
    (user-error "%s" unsaved-message))
  (let* ((prisma (development-standards--require-executable "prisma"))
         (root (and (string-suffix-p "/node_modules/.bin/prisma" prisma)
                    (file-name-directory
                     (substring prisma 0 (- (length "node_modules/.bin/prisma"))))))
         (default-directory
          (if (and root (file-in-directory-p buffer-file-name root))
              root
            (file-name-directory buffer-file-name)))
         (file buffer-file-name))
    (with-temp-buffer
      (let ((status (condition-case err
                        (let ((coding-system-for-read 'utf-8))
                          (call-process prisma nil t nil
                                        command "--schema" file))
                      (error (error-message-string err)))))
        (or (eq status 0)
            (progn
              (display-warning
               'development-standards
               (format "Prisma %s failed (%s) for %s:\n%s"
                       command status file (string-trim (buffer-string))))
              nil))))))

(defun development-standards-prisma-format-file ()
  "Format the current Prisma schema file with Prisma and reload it.
The buffer must be saved first; it is not saved automatically."
  (interactive)
  (when (development-standards--prisma-run
         "format" "Save the Prisma buffer before formatting")
    (revert-buffer t t t)
    (message "Prisma formatted %s" buffer-file-name)))

(defun development-standards-prisma-validate-file ()
  "Validate the current Prisma schema file with Prisma.
The buffer must be saved first; it is not saved automatically."
  (interactive)
  (when (development-standards--prisma-run
         "validate" "Save the Prisma buffer before validation")
    (message "Prisma schema is valid: %s" buffer-file-name)))

(defvar development-standards-sql-bindings-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c F") 'development-standards-sql-format-buffer)
    map)
  "Key bindings for local SQL files.")

(define-minor-mode development-standards-sql-bindings-mode
  "Bind SQL Formatter commands in the current SQL buffer."
  :keymap development-standards-sql-bindings-mode-map)

(defvar development-standards-prisma-bindings-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c F") 'development-standards-prisma-format-file)
    (define-key map (kbd "C-c V") 'development-standards-prisma-validate-file)
    map)
  "Key bindings for local Prisma schema files.")

(define-minor-mode development-standards-prisma-bindings-mode
  "Bind Prisma commands in the current Prisma schema buffer."
  :keymap development-standards-prisma-bindings-mode-map)

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
        (flymake-mode 1)))
    (cond ((string-match-p "\\.\\(sql\\|q\\)\\'" buffer-file-name)
           (development-standards-sql-bindings-mode 1))
          ((string-match-p "\\.prisma\\'" buffer-file-name)
           (development-standards-prisma-bindings-mode 1)))))

(add-hook 'find-file-hook 'development-standards-setup)

;;; development-standards-settings.el ends here
