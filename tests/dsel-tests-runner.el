;;; dsel-tests-runner.el --- Test runner for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides a test runner for the DSel package.

;;; Code:

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'ert)
(package-install 'llm)

(message "Package setup complete")

(require 'ert)
(require 'llm)
(require 'llm-fake)
(require 'dsel)

;; Setup test environment
(defvar dsel-test-llm-provider nil
  "LLM provider used during tests.")

(defvar dsel-test-llm-prompt-to-response-map nil
  "An alist mapping prompt content (or key parts) to desired fake responses.
Each element is (PROMPT-SUBSTRING . RESPONSE-STRING).
The first match is used. Tests can `let`-bind this.")

(defvar dsel-test-llm-chat-response
  (lambda (prompt-struct) ; The lambda now takes the llm-chat-prompt struct
    ;; (message "LLM-FAKE-CHAT-RESPONSE: called with prompt-struct: %S" prompt-struct)
    (let ((current-input-interaction (car (last (llm-chat-prompt-interactions prompt-struct))))
          (current-map (symbol-value 'dsel-test-llm-prompt-to-response-map)))
      ;; (message "LLM-FAKE-CHAT-RESPONSE: current-input-interaction: %S, current-map: %S" current-input-interaction current-map)
      (if current-input-interaction
          (let ((current-input-content (llm-chat-prompt-interaction-content current-input-interaction)))
            ;; (message "LLM-FAKE-CHAT-RESPONSE: current-input-content: %S" current-input-content)
            (cl-loop for mapping in current-map
                     ;; (message "LLM-FAKE-CHAT-RESPONSE: checking mapping: %S against input: %S" mapping current-input-content)
                     when (string-match-p (regexp-quote (car mapping)) current-input-content)
                     do
                     ;; (message "LLM-FAKE-CHAT-RESPONSE: found match for %S, response: %S" (car mapping) (cdr mapping))
                     (cl-return (cdr mapping)) ; Return the matched response from cl-loop
                     finally
                     ;; (message "LLM-FAKE-CHAT-RESPONSE: no match in map (loop finished), using default.")
                     (cl-return "Rationale: Default Fake Rationale\n\nB: default_b\n\nY: default_y")))))) ; Default if loop finishes
  "Function to simulate LLM chat action, potentially using dsel-test-llm-prompt-to-response-map.")

;; dsel-setup-test-environment calls make-llm-fake with this lambda.
;; The make-llm-fake needs to be adjusted to pass the prompt to chat-action-func.
(cl-defmethod llm-chat ((provider llm-fake) prompt &optional multi-output)
  "redefine here to use the prompt struct."
  (when (llm-fake-output-to-buffer provider)
    (with-current-buffer (get-buffer-create (llm-fake-output-to-buffer provider))
      (goto-char (point-max))
      (insert "\nCall to llm-chat\n"  (llm-chat-prompt-to-text prompt) "\n")))
  ;; (message "LLM-FAKE: llm chat called with prompt: %S. Chat-action-func %S" prompt (llm-fake-chat-action-func provider))
  (let ((result
         (if (llm-fake-chat-action-func provider)
             (let* ((f (llm-fake-chat-action-func provider))
                    (result (funcall f prompt)))
               ;; (message "LLM-FAKE: result from chat-action-func: %S" result)
               (pcase (type-of result)
                 ('string result)
                 ('cons (signal (car result) (cdr result)))
                 (_ (error "Incorrect type found in `chat-action-func': %s" (type-of result)))))
           "Sample response from `llm-chat-async'")))
    (setf (llm-chat-prompt-interactions prompt)
          (append (llm-chat-prompt-interactions prompt)
                  (list (make-llm-chat-prompt-interaction :role 'assistant :content result))))
    (if multi-output
        `(:text ,result)
      result)))

(defun dsel-setup-test-environment ()
  "Setup the test environment for DSel."
  (setq max-lisp-eval-depth 1000
        print-level 1000
        print-length 1000
        backtrace-line-length 250
        ert-batch-backtrace-right-margin 250)
  (setq dsel-test-llm-provider (make-llm-fake
                                :output-to-buffer "*dsel-test-llm-fake-output-buffer*"
                                :chat-action-func dsel-test-llm-chat-response))
  (dsel-configure :lm dsel-test-llm-provider
                  :adapter (make-dsel-default-chat-adapter)
                  :log-level 'debug
                  :log-to-messages t)
  (message "DSel test environment setup complete."))

;; Load test files
(require 'dsel-core-tests)
(require 'dsel-adapter-tests)
(require 'dsel-predictors-tests)
(require 'dsel-optimizers-tests)
(require 'dsel-signature-extended-tests)
(require 'dsel-complex-types-tests)
(require 'dsel-signature-tests)

;; Run tests

(defun dsel-run-tests (&optional selector)
  "Run DSel tests interactively matching SELECTOR.
If SELECTOR is nil, run all tests matching \"^dsel-test-\"."
  (interactive)
  (dsel-setup-test-environment)
  (ert-run-tests-interactively (or selector "^dsel-test-")))

(defun dsel-run-tests-batch (&optional selector)
  "Run DSel tests in batch mode matching SELECTOR.
If SELECTOR is nil, run all tests matching \"^dsel-test-\"."
  (dsel-setup-test-environment)
  (ert-run-tests-batch-and-exit (or selector "^dsel-test-")))

(provide 'dsel-tests-runner)
;;; dsel-tests-runner.el ends here
