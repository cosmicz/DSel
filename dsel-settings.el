;;; dsel-settings.el --- Global settings for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides global settings functionality for DSel.

;;; Code:

;;; Settings

(require 'cl-lib)

(defgroup dsel nil
  "Emacs Lisp DSL for interacting with Large Language Models."
  :group 'tools
  :prefix "dsel-")

(defcustom dsel-log-buffer "*DSel Log*"
  "Buffer name for dsel logging output.
Set to nil to disable logging."
  :type '(choice (string :tag "Buffer name")
                 (const :tag "Disabled" nil))
  :group 'dsel)

(defcustom dsel-log-level 'info
  "Minimum level of log messages to display.
Messages with a level below this will not be logged.
The levels, in increasing severity, are:
- debug: Detailed debug information
- info: General information messages
- warning: Warning messages
- error: Error messages"
  :type '(choice (const :tag "Debug" debug)
                 (const :tag "Info" info)
                 (const :tag "Warning" warning)
                 (const :tag "Error" error))
  :group 'dsel)

(defcustom dsel-log-to-messages nil
  "When non-nil, log messages to *Messages* buffer in addition to `dsel-log-buffer'.
This works even when `dsel-log-buffer' is nil, allowing logging to *Messages* only."
  :type 'boolean
  :group 'dsel)

(defvar dsel-settings--lm nil
  "Default `llm.el` provider instance.")

(defvar dsel-settings--adapter nil
  "Default `dsel-adapter` instance.")

(defvar dsel-settings--trace nil
  "List to store (module-instance inputs-alist prediction) tuples.")

(defun dsel-configure (&rest plist)
  "Configure global DSel settings.
PLIST may include the following keywords:
- :lm The default LLM provider to use
- :adapter The default adapter to use
- :log-buffer Name of log buffer (nil to disable logging)
- :log-level Minimum log level to display (debug, info, warning, error)
- :log-to-messages Whether to log to *Messages* buffer"
  (let ((lm (plist-get plist :lm))
        (adapter (plist-get plist :adapter))
        (log-buffer (plist-get plist :log-buffer))
        (log-level (plist-get plist :log-level))
        (log-to-messages (plist-get plist :log-to-messages)))
    (when lm
      (setq dsel-settings--lm lm))
    (when adapter
      (setq dsel-settings--adapter adapter))
    (when (not (eq 'unknown (car (memq :log-buffer plist))))
      (setq dsel-log-buffer log-buffer))
    (when log-level
      (setq dsel-log-level log-level))
    (when (not (eq 'unknown (car (memq :log-to-messages plist))))
      (setq dsel-log-to-messages log-to-messages))))

(defmacro dsel-with-settings (bindings &rest body)
  "Execute BODY with the given SETTINGS temporarily bound.
BINDINGS is a list of (SETTING VALUE) pairs."
  (declare (indent 1))
  `(let ,(mapcar (lambda (b)
                   (let ((var-name (intern (format "dsel-settings--%s" (car b))))
                         (value (cadr b)))
                     (list var-name value)))
                 bindings)
     ,@body))

(defvar dsel--log-level-order
  '((debug . 0)
    (info . 1)
    (warning . 2)
    (error . 3))
  "Alist mapping log levels to numeric values for comparison.")

(defun dsel--log (level format-string &rest args)
  "Internal logging function for dsel.
LEVEL is the log level, one of debug, info, warning, or error.
FORMAT-STRING and ARGS are passed to `format`.
If `dsel-log-buffer` is non-nil, writes to the specified log buffer.
If `dsel-log-to-messages` is non-nil, also logs to *Messages* buffer even if
`dsel-log-buffer` is nil."
  (when (>= (or (cdr (assq level dsel--log-level-order)) 0)
            (or (cdr (assq dsel-log-level dsel--log-level-order)) 0))
    (let ((log-message (apply #'format format-string args))
          (level-str (upcase (symbol-name level)))
          (formatted-entry (format "[%s] [%s] %s"
                                   (format-time-string "%H:%M:%S")
                                   (upcase (symbol-name level))
                                   (apply #'format format-string args))))

      ;; Log to dsel-log-buffer if it's set
      (when dsel-log-buffer
        (with-current-buffer (get-buffer-create dsel-log-buffer)
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert formatted-entry "\n"))))

      ;; Also log to *Messages* if dsel-log-to-messages is set
      (when dsel-log-to-messages
        (message "%s" formatted-entry)))))

(provide 'dsel-settings)
;;; dsel-settings.el ends here
