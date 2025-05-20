;;; dsel-predictors-tests.el --- Tests for dsel predictors  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the dsel-predict and dsel-chain-of-thought functionality.

;;; Code:

(require 'ert)
(require 'dsel)

;; Test basic dsel-predict forward
(ert-deftest dsel-test-predict-basic ()
  "Test the basic `dsel-predict` forward workflow."
  (let* ((sig (dsel-make-signature
               "Echo the input value"
               :name 'test-signature
               :input-fields '((foo . (:type string :desc "")))
               :output-fields '((bar . (:type string :desc "")))))
         (predictor (dsel-make-predict sig :lm dsel-test-llm-provider))
         (expected-raw-response "Rationale: Default Fake Rationale\nB: default_b\nY: default_y")
         (prediction (dsel-forward predictor :foo "hello")))

    ;; (message "DEBUG: Prediction object in test: %S" prediction)
    ;; (message "DEBUG: Prediction fields: %S" (dsel-example-fields prediction))
    ;; (message "DEBUG: Value for 'foo': %S" (dsel-example-field prediction 'foo))


    (should (dsel-prediction-p prediction))
    (should (equal (dsel-prediction-lm-provider prediction)
                   dsel-test-llm-provider))
    (should (equal (dsel-prediction-raw-response prediction)
                   expected-raw-response))
    (should (equal (dsel-example-field prediction 'foo) "hello"))))

;; Test chain-of-thought forward
(ert-deftest dsel-test-chain-of-thought-basic ()
  "Test the basic `dsel-chain-of-thought` forward workflow."
  (let* ((sig (dsel-make-signature
               "Test COT"
               :name 'test-cot
               :input-fields '((x . (:type string :desc "")))
               :output-fields '((y . (:type string :desc "")))))
         (cot (dsel-make-chain-of-thought sig :lm dsel-test-llm-provider))
         (expected-raw-response "Rationale: Default Fake Rationale\nB: default_b\nY: default_y")
         (prediction (dsel-forward cot :x "value")))
    (should (dsel-prediction-p prediction))
    (should (equal (dsel-prediction-lm-provider prediction)
                   dsel-test-llm-provider))
    (should (equal (dsel-prediction-raw-response prediction)
                   expected-raw-response))
    (should (string= (dsel-example-field prediction 'rationale) "Default Fake Rationale"))
    (should (string= (dsel-example-field prediction 'y) "default_y"))
    ))

;; Test chain-of-thought with demos
(ert-deftest dsel-test-chain-of-thought-demos ()
  "Test that demos are properly passed to the chain-of-thought predictor."
  (let* ((sig (dsel-make-signature
               "Test COT with Demos"
               :name 'test-cot-demos
               :input-fields '((input . (:type string :desc "")))
               :output-fields '((output . (:type string :desc "")))))
         ;; Create some example demos
         (demo1 (dsel-make-example :input "sample1" :output "result1"))
         (demo2 (dsel-make-example :input "sample2" :output "result2"))
         (demos (list demo1 demo2))
         ;; Create chain-of-thought with demos
         (cot (dsel-make-chain-of-thought sig 
                                         :lm dsel-test-llm-provider
                                         :demos demos))
         ;; Get the underlying predictor to verify demos were passed
         (predictor (dsel-chain-of-thought-predictor cot)))
    
    ;; Verify demos were correctly passed to the predictor
    (should (= (length (dsel-predict-demos predictor)) 2))
    (should (equal (dsel-predict-demos predictor) demos))
    
    ;; Verify first demo fields
    (let ((first-demo (car (dsel-predict-demos predictor))))
      (should (string= (dsel-example-field first-demo 'input) "sample1"))
      (should (string= (dsel-example-field first-demo 'output) "result1")))
    
    ;; Verify second demo fields
    (let ((second-demo (cadr (dsel-predict-demos predictor))))
      (should (string= (dsel-example-field second-demo 'input) "sample2"))
      (should (string= (dsel-example-field second-demo 'output) "result2")))))

(provide 'dsel-predictors-tests)
;;; dsel-predictors-tests.el ends here
