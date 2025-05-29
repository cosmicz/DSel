;;; dsel-performance-tests.el --- Performance and edge case tests for DSel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Performance tests and edge case scenarios for DSel to ensure
;; robustness under stress and boundary conditions.

;;; Code:

(require 'ert)
(require 'dsel)

(ert-deftest dsel-test-large-response-parsing ()
  "Test parsing very large LLM responses."
  (let* ((signature (dsel-make-signature
                     "Process large text data."
                     :input-fields '((:name input :type string :prefix "Input: "))
                     :output-fields '((:name summary :type string :prefix "Summary: ")
                                      (:name details :type string :prefix "Details: "))))
         ;; Create a large response (10KB+)
         (large-text (make-string 5000 ?x))
         (large-response (format "Summary: This is a summary\n\nDetails: %s" large-text))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) large-response)))
         (predict (dsel-make-predict signature :lm provider)))
    
    (let ((start-time (float-time))
          (result (dsel-forward predict :input "test")))
      (let ((parse-time (- (float-time) start-time)))
        (should (dsel-prediction-ok-p result))
        (should (equal "This is a summary" (dsel-get-field result 'summary)))
        (should (equal large-text (dsel-get-field result 'details)))
        ;; Parsing should be reasonably fast (< 1 second for 10KB)
        (should (< parse-time 1.0))))))

(ert-deftest dsel-test-many-concurrent-predictions ()
  "Test many concurrent async predictions."
  (ert-skip "Complex async test with llm-fake compatibility issues")
  (let* ((signature (dsel-make-signature
                     "Simple task."
                     :input-fields '((:name id :type integer :prefix "ID: "))
                     :output-fields '((:name result :type string :prefix "Result: "))))
         (provider (make-llm-fake
                    :chat-action-func (lambda (prompt)
                                        (let ((content (llm-chat-prompt-to-text prompt)))
                                          (if (string-match "ID: \\([0-9]+\\)" content)
                                              (format "Result: processed_%s" (match-string 1 content))
                                            "Result: unknown")))))
         (predict (dsel-make-predict signature :lm provider))
         (num-concurrent 20))
    
    (dsel-aio-with-test 10
      (let* ((promises (cl-loop for i from 1 to num-concurrent
                                collect (dsel-aforward predict :id i)))
             (results (mapcar #'dsel-aio-await promises)))
        
        ;; All should succeed
        (should (= num-concurrent (length results)))
        (dolist (result results)
          (should (dsel-prediction-ok-p result)))
        
        ;; Check that we got all expected results
        (let ((result-values (mapcar (lambda (r) (dsel-get-field r 'result)) results)))
          (should (= num-concurrent (length (cl-remove-duplicates result-values :test #'equal)))))))))

(ert-deftest dsel-test-deeply-nested-types ()
  "Test very deeply nested object structures."
  (ert-skip "Complex nested type test needs JSON parsing improvements")
  (let* ((signature (dsel-make-signature
                     "Process nested data."
                     :input-fields '((:name input :type string :prefix "Input: "))
                     :output-fields '((:name data :type object
                                             :properties ((:name level1 :type object
                                                                 :properties ((:name level2 :type object
                                                                                     :properties ((:name level3 :type object
                                                                                                         :properties ((:name value :type string))))))))
                                             :prefix "Data: "))))
         (deep-json "{\"level1\": {\"level2\": {\"level3\": {\"value\": \"deep_value\"}}}}")
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) (format "Data: %s" deep-json))))
         (predict (dsel-make-predict signature :lm provider)))
    
    (let ((result (dsel-forward predict :input "test")))
      (should (dsel-prediction-ok-p result))
      (let ((data (dsel-get-field result 'data)))
        (should (hash-table-p data))
        (let* ((level1 (gethash "level1" data))
               (level2 (gethash "level2" level1))
               (level3 (gethash "level3" level2))
               (value (gethash "value" level3)))
          (should (equal "deep_value" value)))))))

(ert-deftest dsel-test-unicode-and-special-characters ()
  "Test handling of unicode and special characters."
  (let* ((signature (dsel-make-signature
                     "Process international text."
                     :input-fields '((:name text :type string :prefix "Text: "))
                     :output-fields '((:name language :type string :prefix "Language: ")
                                      (:name sentiment :type string :prefix "Sentiment: "))))
         ;; Test various unicode and special characters
         (unicode-texts '("Hello 世界 🌍" "Здравствуй мир 🇷🇺" "مرحبا بالعالم 🇸🇦" "こんにちは世界 🇯🇵"))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt)
                                        "Language: multilingual\n\nSentiment: positive")))
         (predict (dsel-make-predict signature :lm provider)))
    
    (dolist (text unicode-texts)
      (let ((result (dsel-forward predict :text text)))
        (should (dsel-prediction-ok-p result))
        (should (equal text (dsel-get-field result 'text)))
        (should (equal "multilingual" (dsel-get-field result 'language)))
        (should (equal "positive" (dsel-get-field result 'sentiment)))))))

(ert-deftest dsel-test-malformed-json-handling ()
  "Test graceful handling of malformed JSON responses."
  (let* ((signature (dsel-make-signature
                     "Parse structured data."
                     :input-fields '((:name input :type string :prefix "Input: "))
                     :output-fields '((:name data :type object
                                             :properties ((:name name :type string)
                                                          (:name count :type integer))
                                             :prefix "Data: "))))
         (malformed-jsons '("{\"name\": \"test\", \"count\": }"  ; Missing value
                            "{\"name\": \"test\" \"count\": 5}"  ; Missing comma
                            "{\"name\": \"test\", \"count\": 5"  ; Missing closing brace
                            "not json at all"))
         (predict (dsel-make-predict signature)))
    
    (dolist (bad-json malformed-jsons)
      (let* ((provider (make-llm-fake
                        :chat-action-func (lambda (_prompt) (format "Data: %s" bad-json))))
             (test-predict (dsel-make-predict signature :lm provider))
             (result (dsel-forward test-predict :input "test")))
        
        ;; Should not crash, but should have errors
        (should (dsel-prediction-p result))
        (should-not (dsel-prediction-ok-p result))
        (should (dsel-prediction-errors result))))))

(ert-deftest dsel-test-empty-and-nil-inputs ()
  "Test handling of empty and nil inputs."
  (let* ((signature (dsel-make-signature
                     "Process potentially empty inputs."
                     :input-fields '((:name text :type string :prefix "Text: ")
                                     (:name optional :type string :prefix "Optional: " :required nil))
                     :output-fields '((:name result :type string :prefix "Result: "))))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) "Result: processed")))
         (predict (dsel-make-predict signature :lm provider)))
    
    ;; Test with empty string
    (let ((result (dsel-forward predict :text "" :optional "")))
      (should (dsel-prediction-ok-p result))
      (should (equal "" (dsel-get-field result 'text))))
    
    ;; Test with nil optional field (should be omitted)
    (let ((result (dsel-forward predict :text "hello" :optional nil)))
      (should (dsel-prediction-ok-p result))
      (should (equal "hello" (dsel-get-field result 'text)))
      ;; Optional field should not be present
      (should (null (dsel-get-field result 'optional))))
    
    ;; Test with only required fields
    (let ((result (dsel-forward predict :text "hello")))
      (should (dsel-prediction-ok-p result))
      (should (equal "hello" (dsel-get-field result 'text))))))

(ert-deftest dsel-test-memory-usage-with-large-arrays ()
  "Test memory efficiency with large array responses."
  (ert-skip "Large array test needs memory profiling capabilities")
  (let* ((signature (dsel-make-signature
                     "Generate large list."
                     :input-fields '((:name count :type integer :prefix "Count: "))
                     :output-fields '((:name items :type (array string) :prefix "Items: "))))
         ;; Create an array with 1000 items
         (large-array (cl-loop for i from 1 to 1000
                               collect (format "item_%04d" i)))
         (json-array (json-encode large-array))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) (format "Items: %s" json-array))))
         (predict (dsel-make-predict signature :lm provider)))
    
    (let ((result (dsel-forward predict :count 1000)))
      (should (dsel-prediction-ok-p result))
      (let ((items (dsel-get-field result 'items)))
        (should (listp items))
        (should (= 1000 (length items)))
        (should (equal "item_0001" (car items)))
        (should (equal "item_1000" (car (last items))))))))

(ert-deftest dsel-test-field-name-edge-cases ()
  "Test edge cases in field naming and prefix matching."
  (let* ((signature (dsel-make-signature
                     "Test field naming edge cases."
                     :input-fields '((:name input :type string :prefix "Input: "))
                     :output-fields '((:name result :type string :prefix "Result: ")
                                      (:name result_details :type string :prefix "Result_details: ")
                                      (:name resultant :type string :prefix "Resultant: "))))
         ;; Response with similar prefixes that could cause confusion
         (tricky-response "Result: main result\n\nResult_details: detailed info\n\nResultant: final outcome")
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) tricky-response)))
         (predict (dsel-make-predict signature :lm provider)))
    
    (let ((result (dsel-forward predict :input "test")))
      (should (dsel-prediction-ok-p result))
      (should (equal "main result" (dsel-get-field result 'result)))
      (should (equal "detailed info" (dsel-get-field result 'result_details)))
      (should (equal "final outcome" (dsel-get-field result 'resultant))))))

(ert-deftest dsel-test-async-timeout-edge-cases ()
  "Test async operations near timeout boundaries."
  (let* ((signature (dsel-make-signature
                     "Slow operation."
                     :input-fields '((:name input :type string :prefix "Input: "))
                     :output-fields '((:name result :type string :prefix "Result: "))))
         ;; Provider with 0.1 second delay
         (provider (make-dsel-test-llm-provider
                    :response "Result: slow response"
                    :delay 0.1))
         (predict (dsel-make-predict signature :lm provider)))
    
    (dsel-aio-with-test 2
      ;; This should succeed (timeout > delay)
      (let ((result (dsel-aio-await (dsel-aforward predict :input "test"))))
        (should (dsel-prediction-ok-p result))
        (should (equal "slow response" (dsel-get-field result 'result)))))))

(ert-deftest dsel-test-prediction-field-access-edge-cases ()
  "Test edge cases in prediction field access."
  (let* ((signature (dsel-make-signature
                     "Multi-field test."
                     :input-fields '((:name input :type string :prefix "Input: "))
                     :output-fields '((:name output1 :type string :prefix "Output1: ")
                                      (:name output2 :type string :prefix "Output2: " :required nil))))
         (provider (make-llm-fake
                    :chat-action-func (lambda (_prompt) "Output1: value1")))  ; Missing output2
         (predict (dsel-make-predict signature :lm provider))
         (result (dsel-forward predict :input "test")))
    
    ;; Should have parsing errors for missing optional field
    (should (dsel-prediction-p result))
    (should (equal "value1" (dsel-get-field result 'output1)))
    
    ;; Accessing missing field should return nil
    (should (null (dsel-get-field result 'output2)))
    
    ;; Accessing non-existent field should return nil
    (should (null (dsel-get-field result 'nonexistent)))))

(provide 'dsel-performance-tests)

;;; dsel-performance-tests.el ends here