;;; git-commit-generator.el --- Generate Git commit messages with DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Cosmin-Octavian C. (cosmicz)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This example demonstrates how to use DSel to create a git commit message generator
;; that analyzes git diffs and suggests appropriate commit messages in conventional
;; commit format.  It includes async execution to handle longer LLM processing times.

;; Please configure DSel to use your desired provider, e.g.:
;; (require 'llm-ollama)  ;; Or openai, etc.
;; (dsel-configure :lm (make-llm-ollama :chat-model "gemma3:12b"))

;;; Code:

(require 'dsel)
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
  "C-c C-g"
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

(defcustom git-commit-generator-predictor-type 'predict
  "Type of predictor to use for commit message generation.
'predict - Use simple prediction without reasoning
'cot     - Use chain-of-thought reasoning for more detailed analysis"
  :type '(choice (const :tag "Simple Prediction" predict)
                 (const :tag "Chain of Thought" cot))
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

;; Add chain of thought reasoning for better commit messages
(dsel-defchain-of-thought git-commit-cot-predictor git-commit-signature
  :rationale-field-name 'reasoning
  :rationale-field-desc "Analyze the diff to understand what changes were made and why"
  :rationale-field-prefix "REASONING:"
  :config '(:temperature 0.4))

(defun git-commit-generator-setup ()
  "Set up the git commit generator for the current git-commit buffer."
  (when-let ((map git-commit-mode-map))
    (define-key map (kbd git-commit-generator-key-map) #'git-commit-generator-generate)))

;;;###autoload
(defun git-commit-generator-generate ()
  "Generate a commit message and insert it into the current commit buffer."
  (interactive)
  (message "Generating commit message...")
  (git-commit-generator--sync-generate))

(defun git-commit-generator--sync-generate ()
  "Generate a commit message synchronously."
  (setq git-commit-generator--current-buffer (current-buffer))
  (let ((diff-stat (shell-command-to-string "git diff --cached --stat"))
        (diff-full (shell-command-to-string "git diff --cached")))
    ;; Truncate diff to avoid token limits
    (setq diff-full (substring diff-full 0
                               (min git-commit-generator-diff-size-limit
                                    (length diff-full))))

    ;; Run prediction in background thread
    (message "Analyzing diff...")
    (let* ((predictor (pcase git-commit-generator-predictor-type
                        ('predict 'git-commit-predictor)
                        ('cot 'git-commit-cot-predictor)
                        (_ 'git-commit-predictor)))
           (predictor (symbol-value predictor))
           (prediction (dsel-forward predictor
                                     :diff-stat diff-stat
                                     :diff-full diff-full)))
      (unless (dsel-prediction-ok-p prediction)
        (message "Error generating commit message: Details:")
        (dsel-prediction-report-errors prediction))
      (run-with-timer
       0 nil
       (lambda ()
         (message "Got prediction")
         (if (dsel-prediction-ok-p prediction)
             (let* ((commit-type (dsel-get-field prediction 'commit-type))
                    (commit-subject (dsel-get-field prediction 'commit-subject))
                    (commit-body (dsel-get-field prediction 'commit-body)))
               (if (not (and commit-type commit-subject commit-body))
                   (message "Error: Not all expected commit fields were present in successful prediction. Type: %S, Subject: %S, Body: %S"
                            commit-type commit-subject commit-body)
                 (let ((commit-message (concat commit-type ": " commit-subject "\n\n" commit-body)))
                   (git-commit-generator--insert-message commit-message)
                   (message "Commit message generated!"))))
           ;; Errors occurred
           (progn
             (message "Error generating commit message. Details:")
             (dsel-prediction-report-errors prediction))))))
    "git-commit-generator"))

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
