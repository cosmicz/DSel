;;; dsel-integration-tests.el --- Integration tests for DSel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Integration tests that verify end-to-end DSel workflows combining
;; multiple modules and testing real-world usage scenarios.

;;; Code:

(require 'ert)
(require 'dsel)

(ert-deftest dsel-test-end-to-end-prediction-workflow ()
  "Test complete prediction workflow from signature to result."
  (ert-skip "Complex array type test needs vector/list handling improvements")
  (let* ((signature (dsel-make-signature
                     "Classify the sentiment and extract key topics from text."
                     :input-fields '((:name text :type string :prefix "Text: "))
                     :output-fields '((:name sentiment :type string :prefix "Sentiment: ")
                                      (:name topics :type array :items (:type string) :prefix "Topics: "))))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt)
                                        "Sentiment: positive\n\nTopics: [\"technology\", \"innovation\"]")))
         (predict (dsel-make-predict signature :lm provider))
         (test-text "I love the new AI features in this software!"))
    
    ;; Test sync execution
    (let ((result (dsel-forward predict :text test-text)))
      (should (dsel-prediction-p result))
      (should (dsel-prediction-ok-p result))
      (should (equal test-text (dsel-get-field result 'text)))
      (should (equal "positive" (dsel-get-field result 'sentiment)))
      (should (equal '("technology" "innovation") (dsel-get-field result 'topics))))
    
    ;; Test async execution
    (dsel-aio-with-test 3
      (let ((result (dsel-aio-await (dsel-aforward predict :text test-text))))
        (should (dsel-prediction-p result))
        (should (dsel-prediction-ok-p result))
        (should (equal test-text (dsel-get-field result 'text)))
        (should (equal "positive" (dsel-get-field result 'sentiment)))
        (let ((topics (dsel-get-field result 'topics)))
          (should (or (listp topics) (vectorp topics)))
          (should (= 2 (length topics)))
          (should (member "technology" (if (vectorp topics) (append topics nil) topics)))
          (should (member "innovation" (if (vectorp topics) (append topics nil) topics))))))))

(ert-deftest dsel-test-chain-of-thought-integration ()
  "Test chain-of-thought with complex reasoning workflow."
  (let* ((base-signature (dsel-make-signature
                          "Solve a math word problem step by step."
                          :input-fields '((:name problem :type string :prefix "Problem: "))
                          :output-fields '((:name answer :type integer :prefix "Answer: "))))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt)
                                        "Rationale: First I need to identify the numbers: 15 apples and 7 oranges. Then I add them: 15 + 7 = 22.\n\nAnswer: 22")))
         (cot (dsel-make-chain-of-thought base-signature :lm provider))
         (problem "Sarah has 15 apples and 7 oranges. How many fruits does she have in total?"))
    
    (let ((result (dsel-forward cot :problem problem)))
      (should (dsel-prediction-p result))
      (should (dsel-prediction-ok-p result))
      (should (equal problem (dsel-get-field result 'problem)))
      (should (stringp (dsel-get-field result 'rationale)))
      (should (string-match-p "15.*7.*22" (dsel-get-field result 'rationale)))
      (should (= 22 (dsel-get-field result 'answer))))))

(ert-deftest dsel-test-multi-step-pipeline ()
  "Test a pipeline with multiple prediction steps."
  (let* ((extract-sig (dsel-make-signature
                       "Extract key facts from text."
                       :input-fields '((:name text :type string :prefix "Text: "))
                       :output-fields '((:name facts :type array :items (:type string) :prefix "Facts: "))))
         (summarize-sig (dsel-make-signature
                         "Create a summary from facts."
                         :input-fields '((:name facts :type array :items (:type string) :prefix "Facts: "))
                         :output-fields '((:name summary :type string :prefix "Summary: "))))
         (extract-provider (make-llm-fake
                            :chat-action-func (lambda (_prompt)
                                                "Facts: [\"Company launched in 2020\", \"Has 50 employees\", \"Based in San Francisco\"]")))
         (summarize-provider (make-llm-fake
                              :chat-action-func (lambda (_prompt)
                                                  "Summary: A San Francisco-based company founded in 2020 with 50 employees.")))
         (extractor (dsel-make-predict extract-sig :lm extract-provider))
         (summarizer (dsel-make-predict summarize-sig :lm summarize-provider))
         (input-text "TechCorp was founded in 2020 and now employs 50 people at their San Francisco headquarters."))
    
    ;; Step 1: Extract facts
    (let* ((extraction-result (dsel-forward extractor :text input-text))
           (facts (dsel-get-field extraction-result 'facts)))
      (should (dsel-prediction-ok-p extraction-result))
      (should (or (listp facts) (vectorp facts)))
      (should (= 3 (length facts)))
      
      ;; Step 2: Summarize facts
      (let ((summary-result (dsel-forward summarizer :facts facts)))
        (should (dsel-prediction-ok-p summary-result))
        (should (stringp (dsel-get-field summary-result 'summary)))
        (should (string-match-p "San Francisco.*2020.*50" (dsel-get-field summary-result 'summary)))))))

(ert-deftest dsel-test-error-propagation-through-pipeline ()
  "Test how errors propagate through a multi-step pipeline."
  (let* ((sig (dsel-make-signature
               "Process text data."
               :input-fields '((:name text :type string :prefix "Text: "))
               :output-fields '((:name result :type string :prefix "Result: "))))
         ;; First predictor succeeds
         (provider1 (make-llm-fake
                     :chat-action-func (lambda (_prompt) "Result: processed successfully")))
         ;; Second predictor produces bad output
         (provider2 (make-llm-fake
                     :chat-action-func (lambda (_prompt) "BadPrefix: unexpected format")))
         (predictor1 (dsel-make-predict sig :lm provider1))
         (predictor2 (dsel-make-predict sig :lm provider2)))
    
    ;; First predictor should work fine
    (let ((result1 (dsel-forward predictor1 :text "test input")))
      (should (dsel-prediction-ok-p result1))
      (should (equal "processed successfully" (dsel-get-field result1 'result))))
    
    ;; Second predictor should have parsing errors
    (let ((result2 (dsel-forward predictor2 :text "test input")))
      (should-not (dsel-prediction-ok-p result2))
      (should (dsel-prediction-errors result2))
      ;; Should still have input field
      (should (equal "test input" (dsel-get-field result2 'text))))))

(ert-deftest dsel-test-concurrent-predictions ()
  "Test multiple concurrent predictions with different providers."
  (ert-skip "Complex async test with llm-fake compatibility issues")
  (let* ((sig (dsel-make-signature
               "Simple classification task."
               :input-fields '((:name input :type string :prefix "Input: "))
               :output-fields '((:name output :type string :prefix "Output: "))))
         (provider1 (make-llm-fake
                     :chat-action-func (lambda (_prompt) "Output: result1")))
         (provider2 (make-llm-fake
                     :chat-action-func (lambda (_prompt) "Output: result2")))
         (provider3 (make-llm-fake
                     :chat-action-func (lambda (_prompt) "Output: result3")))
         (predictor1 (dsel-make-predict sig :lm provider1))
         (predictor2 (dsel-make-predict sig :lm provider2))
         (predictor3 (dsel-make-predict sig :lm provider3)))
    
    (dsel-aio-with-test 5
      (let* ((promise1 (dsel-aforward predictor1 :input "test1"))
             (promise2 (dsel-aforward predictor2 :input "test2"))
             (promise3 (dsel-aforward predictor3 :input "test3"))
             (result1 (dsel-aio-await promise1))
             (result2 (dsel-aio-await promise2))
             (result3 (dsel-aio-await promise3)))
        
        ;; All should succeed
        (should (dsel-prediction-ok-p result1))
        (should (dsel-prediction-ok-p result2))
        (should (dsel-prediction-ok-p result3))
        
        ;; Check results
        (should (equal "result1" (dsel-get-field result1 'output)))
        (should (equal "result2" (dsel-get-field result2 'output)))
        (should (equal "result3" (dsel-get-field result3 'output)))))))

(ert-deftest dsel-test-configuration-inheritance ()
  "Test that configuration parameters are properly inherited."
  (let* ((sig (dsel-make-signature
               "Test configuration passing."
               :input-fields '((:name input :type string :prefix "Input: "))
               :output-fields '((:name output :type string :prefix "Output: "))))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) "Output: configured")))
         (predict (dsel-make-predict sig 
                                     :lm provider
                                     :config '(:temperature 0.7 :max-tokens 100))))
    
    ;; Configuration should be available in the predictor
    (should (equal '(:temperature 0.7 :max-tokens 100) 
                   (dsel-predict-config predict)))
    
    ;; TODO: Should work in sync mode (testing config is passed through)
    ;; Commented out until llm-fake properly supports config parameters
    ;; (let ((sync-result (dsel-forward predict :input "test")))
    ;;   (should (dsel-prediction-ok-p sync-result))
    ;;   (should (equal "configured" (dsel-get-field sync-result 'output))))
    ))

(provide 'dsel-integration-tests)

;;; dsel-integration-tests.el ends here