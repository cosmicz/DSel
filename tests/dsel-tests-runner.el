;;; dsel-tests-runner.el --- Test runner for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides a test runner for the DSel package.

;;; Code:

(require 'ert)

;; Make sure llm.el is available
(unless (require 'llm nil t)
  (error "DSel tests require llm.el to be available"))

(require 'dsel)

;; Check if llm-fake is available
(unless (require 'llm-fake nil t)
  (message "Warning: llm-fake not available. Some tests may fail or be skipped."))

;; Setup test environment
(defvar dsel-test-llm-provider nil
  "LLM provider used during tests.")

(defun dsel-setup-test-environment ()
  "Setup the test environment for DSel tests."
  (when (featurep 'llm-fake)
    (setq dsel-test-llm-provider (llm-fake-provider-create))
    (dsel-configure :lm dsel-test-llm-provider 
                   :adapter (make-dsel-default-chat-adapter))
    (message "DSel test environment setup complete.")))

;; Load test files
(require 'dsel-core-tests)
(require 'dsel-adapter-tests)

;; Run tests

(defun dsel-run-tests ()
  "Run all DSel tests interactively."
  (interactive)
  (dsel-setup-test-environment)
  (ert-run-tests-interactively "^dsel-test-"))

(defun dsel-run-tests-batch ()
  "Run all DSel tests in batch mode."
  (dsel-setup-test-environment)
  (ert-run-tests-batch-and-exit "^dsel-test-"))

(provide 'dsel-tests-runner)
;;; dsel-tests-runner.el ends here