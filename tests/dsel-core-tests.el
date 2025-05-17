;;; dsel-core-tests.el --- Tests for dsel core functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains ERT tests for the dsel core functionality.

;;; Code:

(require 'ert)
(require 'dsel)

;; Stub for llm.el when running tests

(defvar llm-test-stub-active nil
  "When non-nil, uses stub implementations for LLM functions.")

(when llm-test-stub-active
  (defun llm-make-chat-prompt (&rest plist)
    "Create a chat prompt structure from PLIST for testing."
    (append (list :type 'llm-chat) plist))

  (defun llm-chat (provider prompt &optional config)
    "Simulate an LLM chat with PROVIDER using PROMPT and optional CONFIG.
Returns a stubbed response for testing."
    (format "Sentiment: positive\nConfidence: 0.95")))

;; Test dsel-signature

(ert-deftest dsel-test-signature ()
  "Test creation and behavior of dsel-signature."
  (let* ((instructions "Classify the sentiment of the text.")
         (sig (dsel-make-signature
               instructions
               :name 'sentiment-classifier
               :input-fields '((text . (:type string :desc "The text to classify")))
               :output-fields '((sentiment . (:type string :desc "The sentiment: positive, negative, or neutral"))))))
    
    ;; Test structure
    (should (dsel-signature-p sig))
    (should (eq (dsel-signature-name sig) 'sentiment-classifier))
    (should (string= (dsel-signature-instructions sig) instructions))
    
    ;; Test field access
    (should (= (length (dsel-signature-input-fields sig)) 1))
    (should (= (length (dsel-signature-output-fields sig)) 1))
    
    ;; Test default prefix generation
    (let ((text-field (cdr (assq 'text (dsel-signature-input-fields sig))))
          (sentiment-field (cdr (assq 'sentiment (dsel-signature-output-fields sig)))))
      (should (string= (plist-get text-field :prefix) "Text: "))
      (should (string= (plist-get sentiment-field :prefix) "Sentiment: ")))))

;; Test dsel-example

(ert-deftest dsel-test-example ()
  "Test creation and manipulation of dsel-example."
  (let* ((example (dsel-make-example
                  :text "I love this product!"
                  :sentiment "positive"))
         (input-example (dsel-example-with-inputs example 'text)))
    
    ;; Test structure
    (should (dsel-example-p example))
    (should (equal (dsel-example-field example 'text) "I love this product!"))
    (should (equal (dsel-example-field example 'sentiment) "positive"))
    
    ;; Test input/label separation
    (should (= (length (dsel-example-input-keys input-example)) 1))
    (should (equal (dsel-example-inputs input-example)
                  '((text . "I love this product!"))))
    (should (equal (dsel-example-labels input-example)
                  '((sentiment . "positive"))))
    
    ;; Test field modification
    (dsel-set-example-field example 'sentiment "very positive")
    (should (equal (dsel-example-field example 'sentiment) "very positive"))))

;; Test dsel-prediction

(ert-deftest dsel-test-prediction ()
  "Test creation and behavior of dsel-prediction."
  (let* ((fake-provider "fake-llm")
         (prediction (dsel-make-prediction
                     :text "I love this product!"
                     :sentiment "positive"
                     :lm-provider fake-provider
                     :raw-response "Sentiment: positive")))
    
    ;; Test structure
    (should (dsel-prediction-p prediction))
    (should (dsel-example-p prediction)) ; Should also be an example
    (should (equal (dsel-prediction-lm-provider prediction) fake-provider))
    (should (equal (dsel-prediction-raw-response prediction) "Sentiment: positive"))
    
    ;; Test field access
    (should (equal (dsel-example-field prediction 'text) "I love this product!"))
    (should (equal (dsel-example-field prediction 'sentiment) "positive"))))

(provide 'dsel-core-tests)
;;; dsel-core-tests.el ends here