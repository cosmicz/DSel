;;; dsel-settings.el --- Global settings for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides global settings functionality for DSel.

;;; Code:

;;; Settings

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
- :adapter The default adapter to use"
  (let ((lm (plist-get plist :lm))
        (adapter (plist-get plist :adapter)))
    (when lm
      (setq dsel-settings--lm lm))
    (when adapter
      (setq dsel-settings--adapter adapter))))

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

(provide 'dsel-settings)
;;; dsel-settings.el ends here