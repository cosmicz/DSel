;;; sembr-reflow.el --- Reflow paragraphs with semantic line breaks using DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Cosmin-Octavian C. (cosmicz)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This example demonstrates how to use DSel to reflow paragraphs according to
;; the Semantic Line Breaks (SemBr) specification (https://sembr.org/).
;;
;; SemBr helps improve readability and version control by breaking lines at
;; semantic boundaries rather than arbitrary character counts.

;;; Code:

(require 'dsel)
(require 'dsel-aio)
(require 'dsel-predictors)

;; Define the signature for semantic line break reflow
(dsel-defsignature sembr-signature
    "Reflow the given paragraph using semantic line breaks according to SemBr specification.

You MUST follow these exact rules:
1. ALWAYS break after complete sentences ending with periods (.), exclamation marks (!), or question marks (?)
2. SHOULD break after independent clauses marked by commas (,), semicolons (;), colons (:), or em dashes (—)
3. MAY break after dependent clauses to improve clarity
4. NEVER break in the middle of words or phrases
5. Keep line length under 80 characters when possible
6. Preserve all original text exactly - do not change, add, or remove any words "
  :input-fields '((:name paragraph
                         :type string
                         :desc "The paragraph to reflow with semantic line breaks"
                         :prefix "PARAGRAPH:"))
  :output-fields '((:name reflowed
                          :type string
                          :desc "The same paragraph with line breaks at semantic boundaries"
                          :prefix "REFLOWED:")))

;; Define examples for better reflow accuracy
(dsel-defexamples sembr-examples '(paragraph)
  ;; Example 1: Multiple sentences
  (:paragraph
   "All human beings are born free and equal in dignity and rights. They are endowed with reason and conscience and should act towards one another in a spirit of brotherhood."
   :reflowed
   "All human beings are born free and equal in dignity and rights.
They are endowed with reason and conscience
and should act towards one another in a spirit of brotherhood.")
  ;; Example 2: Complex sentence with clauses
  (:paragraph
   "The quick brown fox jumps over the lazy dog, demonstrating a pangram that contains all letters of the alphabet; however, this particular sentence is quite long and would benefit from semantic line breaks."
   :reflowed
   "The quick brown fox jumps over the lazy dog,
demonstrating a pangram that contains all letters of the alphabet;
however,
this particular sentence is quite long
and would benefit from semantic line breaks.")
  ;; Example 3: Short text that shouldn't be broken
  (:paragraph
   "This is a short sentence."
   :reflowed
   "This is a short sentence."))

;; Create a predictor for SemBr reflow with examples
(dsel-defpredict sembr-predictor sembr-signature
  :config '(:temperature 0.1)    ; Low temperature for consistent formatting
  :demos sembr-examples)          ; Use examples for better accuracy

;;;###autoload
(defun sembr-reflow-region (start end)
  "Reflow the region from START to END using semantic line breaks."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties start end))
         (trimmed-text (string-trim original-text)))

    (if (string-blank-p trimmed-text)
        (message "Empty region")

      (message "Reflowing region...")
      (dsel-aio-then
       (dsel-aforward sembr-predictor :paragraph trimmed-text)
       ;; Success callback
       (lambda (prediction)
         (if (dsel-prediction-ok-p prediction)
             (let ((reflowed (dsel-get-field prediction 'reflowed)))
               (if (and reflowed (not (string-blank-p reflowed)))
                   (progn
                     ;; Preserve any leading/trailing whitespace from original
                     (let ((leading-space (and (string-match "^\\s-*" original-text)
                                               (match-string 0 original-text)))
                           (trailing-space (and (string-match "\\s-*$" original-text)
                                                (match-string 0 original-text))))
                       (delete-region start end)
                       (goto-char start)
                       (insert (or leading-space "")
                               (string-trim reflowed)
                               (or trailing-space ""))
                       (message "Region reflowed with semantic line breaks")))
                 (message "No valid reflowed text in prediction")))
           (message "Error reflowing region")
           (dsel-prediction-report-errors prediction)))
       ;; Error callback
       (lambda (err)
         (message "Failed to reflow: %s" (if (listp err) (cadr err) err)))))))

;;;###autoload
(defun sembr-reflow-paragraph ()
  "Reflow the current paragraph using semantic line breaks."
  (interactive)
  (save-excursion
    (let* ((paragraph-start (progn (backward-paragraph) (point)))
           (paragraph-end (progn (forward-paragraph) (point)))
           (paragraph-text (buffer-substring-no-properties paragraph-start paragraph-end))
           (trimmed-text (string-trim paragraph-text))
           )

      (if (string-blank-p trimmed-text)
          (message "No paragraph at point")

        (message "Reflowing paragraph...")
        (dsel-aio-then
         (dsel-aforward sembr-predictor :paragraph trimmed-text)
         ;; Success callback
         (lambda (prediction)
           (if (dsel-prediction-ok-p prediction)
               (let ((reflowed (dsel-get-field prediction 'reflowed)))
                 (if (and reflowed (not (string-blank-p reflowed)))
                     (progn
                       ;; Preserve any leading/trailing whitespace
                       (let ((leading-space (and (string-match "^\\s-*" paragraph-text)
                                                 (match-string 0 paragraph-text)))
                             (trailing-space (and (string-match "\\s-*$" paragraph-text)
                                                  (match-string 0 paragraph-text))))
                         (delete-region paragraph-start paragraph-end)
                         (goto-char paragraph-start)
                         (insert (or leading-space "")
                                 (string-trim reflowed)
                                 (or trailing-space ""))
                         (message "Paragraph reflowed with semantic line breaks")))
                   (message "No valid reflowed text in prediction")))
             (message "Error reflowing paragraph")
             (dsel-prediction-report-errors prediction)))
         ;; Error callback
         (lambda (err)
           (message "Failed to reflow: %s" (if (listp err) (cadr err) err))))))))

;; Integration with fill commands
;;;###autoload
(defun sembr-fill-paragraph ()
  "Fill paragraph using semantic line breaks instead of fixed column."
  (interactive)
  (sembr-reflow-paragraph))

;; Key binding suggestions
(defvar sembr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c s p") #'sembr-reflow-paragraph)
    (define-key map (kbd "C-c s r") #'sembr-reflow-region)
    map)
  "Keymap for semantic line break commands.")

;;;###autoload
(define-minor-mode sembr-mode
  "Minor mode for semantic line break paragraph reflow.
\\{sembr-mode-map}"
  :lighter " SemBr"
  :keymap sembr-mode-map)

(provide 'sembr-reflow)
;;; sembr-reflow.el ends here
