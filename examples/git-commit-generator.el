;;; git-commit-generator.el --- Generate Git commit messages with DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Cosmin-Octavian C. (cosmicz)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This example demonstrates how to use DSel to create a git commit message generator
;; that analyzes git diffs and suggests appropriate commit messages in conventional
;; commit format. It showcases the new dsel-defmodule and dsel-defaforward macros
;; for creating clean, async-first LLM applications.

;; Please configure DSel to use your desired provider, e.g.:
;; (require 'llm-ollama)  ;; Or openai, etc.
;; (dsel-configure :lm (make-llm-ollama :chat-model "gemma3:12b"))

;; Key features demonstrated:
;; - dsel-defmodule: Create a modular commit message generator
;; - dsel-defaforward: Define async methods for non-blocking execution
;; - dsel-aio-then: Handle async results with success and error callbacks
;; - Robust error handling for both prediction errors and promise rejections
;; - Input validation (checking for staged changes)
;; - Clean separation of concerns with helper functions
;; - Integration with git-commit-mode for seamless workflow

;; Usage:
;; - M-x git-commit-generator-generate (async, non-blocking)

;;; Code:

(require 'dsel)
(require 'dsel-aio)
(require 'dsel-predictors)
(require 'magit nil t)
(require 'transient nil t)

(defgroup git-commit-generator nil
  "Generate git commit messages using LLMs."
  :group 'tools
  :prefix "git-commit-generator-")

(defcustom git-commit-generator-examples-file
  (expand-file-name "git-commit-examples.el" user-emacs-directory)
  "File to store collected examples of good commit messages."
  :type 'string
  :group 'git-commit-generator)

(defcustom git-commit-generator-key-map
  "C-c g"
  "Key binding to trigger commit message generation in git-commit-mode."
  :type 'string
  :group 'git-commit-generator)

(defcustom git-commit-generator-diff-size-limit 50000
  "Maximum number of characters to include from the diff.
Larger diffs will be truncated to avoid token limit issues."
  :type 'integer
  :group 'git-commit-generator)

(defcustom git-commit-generator-commit-types
  ["feat" "fix" "docs" "style" "refine" "test" "chore" "build" "ci" "perf" "refactor"]
  "Valid commit types for conventional commits."
  :type '(vector string)
  :group 'git-commit-generator)


(defvar git-commit-generator-collected-examples '()
  "List of collected examples for git commit message generation.")

(defvar git-commit-generator--current-buffer nil
  "Buffer where the commit message should be inserted.")

;; Define the signature for our git commit message generator
(dsel-defsignature git-commit-signature
    "Generate a commit message in conventional commits format from a git diff."
  :input-fields
  '((:name diff-stat
           :type string
           :desc "Output of git diff --cached --stat"
           :prefix "DIFF STATS:")
    (:name diff-full
           :type string
           :desc "Output of git diff --cached"
           :prefix "FULL DIFF:"))
  :output-fields
  `((:name commit-type
           :type string
           :desc "The type of the commit (feat, fix, etc.)"
           :prefix "COMMIT TYPE:"
           :enum ,git-commit-generator-commit-types)
    (:name commit-subject
           :type string
           :desc "The subject line of the commit message"
           :prefix "COMMIT SUBJECT:")
    (:name commit-body
           :type string
           :desc "The body of the commit message (1-5 lines)"
           :prefix "COMMIT BODY:")))

;; Create a predictor for commit message generation
(dsel-defpredict git-commit-predictor git-commit-signature
  :config '(:temperature 0.3))

;; Define a simplified commit generator module
(dsel-defmodule git-commit-generator-module
    "A module that generates git commit messages from git diffs."
  :submodules
  ((predictor git-commit-predictor)))

;; Define the async forward method for the commit generator
(dsel-defaforward git-commit-generator-module (&key diff-stat diff-full)
  "Generate a commit message from git diff information."
  (let ((predictor (cdr (assq 'predictor submodules))))
    (dsel-aio-await (dsel-aforward predictor
                                   :diff-stat diff-stat
                                   :diff-full diff-full))))

(defun git-commit-generator-setup ()
  "Set up the git commit generator for the current git-commit buffer."
  (when-let ((map git-commit-mode-map))
    (define-key map (kbd git-commit-generator-key-map) #'git-commit-generator-generate)))

;;;###autoload
(defun git-commit-generator-generate ()
  "Generate a commit message and insert it into the current commit buffer."
  (interactive)
  (message "Generating commit message...")
  (git-commit-generator--generate))

(defun git-commit-generator--generate ()
  "Generate a commit message using dsel-aio-then."
  (setq git-commit-generator--current-buffer (current-buffer))

  ;; Get git diff information
  (let ((diff-stat (shell-command-to-string "git diff --cached --stat"))
        (diff-full (shell-command-to-string "git diff --cached")))

    ;; Validate that there are changes to commit
    (if (or (string-empty-p (string-trim diff-stat))
            (string-empty-p (string-trim diff-full)))
        (message "No staged changes found. Stage some changes first with 'git add'.")

      ;; Truncate diff to avoid token limits
      (when (> (length diff-full) git-commit-generator-diff-size-limit)
        (setq diff-full (substring diff-full 0 git-commit-generator-diff-size-limit))
        (message "Large diff truncated to %d characters" git-commit-generator-diff-size-limit))

      ;; Create module and generate commit message asynchronously
      (message "Analyzing %d files..." (length (split-string diff-stat "\\n" t)))
      (let ((module (make-git-commit-generator-module)))
        (dsel-aio-then
         (dsel-aforward module
                        :diff-stat diff-stat
                        :diff-full diff-full)
         ;; Success callback
         (lambda (prediction)
           (if (dsel-prediction-ok-p prediction)
             (git-commit-generator--process-successful-prediction prediction)
           (git-commit-generator--handle-prediction-errors prediction)))
         ;; Error callback for promise rejections
         (lambda (err)
           (message "Failed to generate commit message: %s"
                  (if (listp err) (cadr err) err)))))))

  (defun git-commit-generator--process-successful-prediction (prediction)
    "Process a successful prediction and insert the commit message."
    (let ((commit-type (dsel-get-field prediction 'commit-type))
          (commit-subject (dsel-get-field prediction 'commit-subject))
          (commit-body (dsel-get-field prediction 'commit-body)))

      ;; Validate that we got all required fields
      (if (not (and commit-type commit-subject))
          (message "Incomplete commit message generated. Missing: %s"
                   (string-join
                  (delq nil (list (unless commit-type "type")
                                  (unless commit-subject "subject")))
                  ", "))

        ;; Build and insert the commit message
        (let ((commit-message
               (if (and commit-body (not (string-empty-p (string-trim commit-body))))
                 (format "%s: %s\n\n%s" commit-type commit-subject commit-body)
               (format "%s: %s" commit-type commit-subject))))

          (git-commit-generator--insert-message commit-message)
          (message "Generated: \"%s: %s\"" commit-type commit-subject)))))

  (defun git-commit-generator--handle-prediction-errors (prediction)
    "Handle prediction errors and report them to the user."
    (message "Error generating commit message:")
    (dsel-prediction-report-errors prediction)
    (message "Try staging fewer files or simplifying changes if the issue persists."))

  (defun git-commit-generator--insert-message (commit-message)
    "Insert COMMIT-MESSAGE into the commit buffer."
    (when (and git-commit-generator--current-buffer
               (buffer-live-p git-commit-generator--current-buffer))
      (with-current-buffer git-commit-generator--current-buffer
        (insert commit-message)
        (insert "\n\n")
        (goto-char (point-min))
        ;; If magit is available, refresh the commit buffer
        (when (fboundp 'magit-refresh)
          (magit-refresh)))))

;;;###autoload
  (defun git-commit-generator-save-example (commit-hash)
    "Save a good commit as an example for future training.
COMMIT-HASH should be the hash of the commit to use as an example."
    (interactive "sCommit hash: ")
    (let* ((commit-message (shell-command-to-string (format "git log -n 1 --pretty=format:%%B %s" commit-hash)))
           (diff-stat (shell-command-to-string (format "git show --stat %s" commit-hash)))
           (diff-full (shell-command-to-string (format "git show %s" commit-hash)))
           (example (list :commit-message commit-message
                        :diff-stat diff-stat
                        :diff-full diff-full
                        :commit-hash commit-hash)))
      (push example git-commit-generator-collected-examples)
      (message "Example saved from commit %s" commit-hash)))

;;;###autoload
  (defun git-commit-generator-save-examples ()
    "Save collected examples to the configured file."
    (interactive)
    (with-temp-file git-commit-generator-examples-file
      (insert ";; Git commit examples for training\n\n")
      (insert "(setq git-commit-generator-collected-examples\n  '")
      (pp git-commit-generator-collected-examples (current-buffer))
      (insert ")\n"))
    (message "Saved %d examples to %s"
             (length git-commit-generator-collected-examples)
             git-commit-generator-examples-file))

;;;###autoload
  (defun git-commit-generator-load-examples ()
    "Load examples from the configured file."
    (interactive)
    (when (file-exists-p git-commit-generator-examples-file)
      (load git-commit-generator-examples-file)
      (message "Loaded %d examples from %s"
               (length git-commit-generator-collected-examples)
               git-commit-generator-examples-file)))

  (provide 'git-commit-generator)
;;; git-commit-generator.el ends here
