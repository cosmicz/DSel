;;; dsel-adapter-tests.el --- Tests for dsel adapter functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains ERT tests for the dsel adapter functionality.

;;; Code:

(require 'ert)
(require 'dsel)

;; Test dsel-adapter

(ert-deftest dsel-test-adapter-format-prompt ()
  "Test prompt formatting with dsel-adapter."
  (let* ((instructions "Classify the sentiment of the text.")
         (sig (dsel-make-signature
               instructions
               :name 'sentiment-classifier
               :input-fields '((text . (:type string :desc "The text to classify")))
               :output-fields '((sentiment . (:type string :desc "The sentiment: positive, negative, or neutral")))))
         (adapter (make-dsel-default-chat-adapter))
         (demos (list
                 (dsel-example-with-inputs
                  (dsel-make-example
                   :text "I love this product!"
                   :sentiment "positive")
                  'text)
                 (dsel-example-with-inputs
                  (dsel-make-example
                   :text "I hate this product!"
                   :sentiment "negative")
                  'text)))
         (inputs '((text . "This product is okay.")))
         (prompt (dsel-adapter-format-prompt adapter sig demos inputs)))
    
    ;; Test prompt structure
    (should (llm-chat-prompt-p prompt))
    (should (stringp (llm-chat-prompt-context prompt)))
    (let ((system-message-content (llm-chat-prompt-context prompt)))
      (should (string-match-p (regexp-quote instructions) system-message-content))
      (should (string-match-p "Your input fields are:" system-message-content))
      (should (string-match-p "Your output fields are:" system-message-content)))

    (should (listp (llm-chat-prompt-examples prompt)))
    (should (= (length (llm-chat-prompt-examples prompt)) 2))

    ;; Test the :interactions slot (which initially holds the current user input)
    ;; Note: llm-provider-utils-combine-to-system-prompt (called later by actual providers)
    ;; will merge :context and :examples into this :interactions list.
    ;; What llm-make-chat-prompt does is put the `content` argument into :interactions.
    (should (listp (llm-chat-prompt-interactions prompt)))
    (let ((initial-interactions (llm-chat-prompt-interactions prompt)))
      (should (= (length initial-interactions) 1)) ; Only the current input initially
      (let ((current-user-interaction (car initial-interactions)))
        (should (eq (llm-chat-prompt-interaction-role current-user-interaction) 'user))
        (should (string-match-p "This product is okay"
                                (llm-chat-prompt-interaction-content current-user-interaction)))))))

(ert-deftest dsel-test-adapter-parse-output ()
  "Test output parsing with dsel-adapter."
  (let* ((sig (dsel-make-signature
               "Classify the sentiment of the text."
               :name 'sentiment-classifier
               :input-fields '((text . (:type string :desc "The text to classify")))
               :output-fields '((sentiment . (:type string :desc "The sentiment: positive, negative, or neutral"))
                                (confidence . (:type number :desc "Confidence score from 0 to 1")))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Sentiment: positive\nConfidence: 0.95")
         (result (dsel-adapter-parse-output adapter sig response)))
    
    ;; Test parsed result
    (should (listp result))
    (should (= (length result) 2))
    (should (equal (assq 'sentiment result) '(sentiment . "positive")))
    (should (equal (assq 'confidence result) '(confidence . 0.95)))))

(provide 'dsel-adapter-tests)
;;; dsel-adapter-tests.el ends here
