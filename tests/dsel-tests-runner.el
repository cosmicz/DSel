;;; dsel-tests-runner.el --- Test runner for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
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

(defun dsel-setup-test-environment ()
  "Setup the test environment for DSel tests."
  (setq dsel-test-llm-provider (llm-fake-provider-create))
  (dsel-configure :lm dsel-test-llm-provider
                  :adapter (make-dsel-default-chat-adapter))
  (message "DSel test environment setup complete."))

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
