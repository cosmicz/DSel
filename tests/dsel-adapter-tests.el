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
    (should (eq (plist-get prompt :type) 'llm-chat))
    (should (stringp (plist-get prompt :system)))
    (should (listp (plist-get prompt :messages)))
    
    ;; Test system message contains instructions
    (let ((system (plist-get prompt :system)))
      (should (string-match-p (regexp-quote instructions) system)))
    
    ;; Test messages structure
    (let ((messages (plist-get prompt :messages)))
      ;; Should have 5 messages (2 demos x 2 messages each + 1 user input)
      (should (= (length messages) 5))
      ;; User messages
      (should (string-match-p "I love this product" 
                             (plist-get (nth 0 messages) :content)))
      (should (string-match-p "I hate this product" 
                             (plist-get (nth 2 messages) :content)))
      ;; Assistant messages
      (should (string-match-p "positive" 
                             (plist-get (nth 1 messages) :content)))
      (should (string-match-p "negative" 
                             (plist-get (nth 3 messages) :content)))
      ;; Current input
      (should (string-match-p "This product is okay" 
                             (plist-get (nth 4 messages) :content))))))

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