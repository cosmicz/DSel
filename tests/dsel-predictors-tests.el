;;; dsel-predictors-tests.el --- Tests for dsel predictors  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

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
               :input-fields (list '(:name foo :type string :desc "Input foo"))
               :output-fields (list '(:name bar :type string :desc "Output bar" :prefix "Bar:")))) ; Prefix without trailing space
         (predictor (dsel-make-predict sig :lm dsel-test-llm-provider))
         ;; Set up the expected response for this specific test case via the map
         (dsel-test-llm-prompt-to-response-map `((,(dsel--format-input-fields sig '((foo . "hello"))) . "Bar:world\n\n")))
         (expected-raw-response "Bar:world\n\n") ; The exact response we expect
         (prediction (dsel-forward predictor :foo "hello")))

    (should (dsel-prediction-p prediction))
    (should (equal (dsel-prediction-lm-provider prediction)
                   dsel-test-llm-provider))
    (should (equal (dsel-prediction-raw-response prediction)
                   expected-raw-response))
    (should (equal (dsel-get-field prediction 'foo) "hello"))
    (should (equal (dsel-get-field prediction 'bar) "world"))))

;; Test chain-of-thought forward
(ert-deftest dsel-test-chain-of-thought-basic ()
  "Test the basic `dsel-chain-of-thought` forward workflow."
  (let* ((sig (dsel-make-signature
               "Test COT"
               :name 'test-cot
               :input-fields (list '(:name x :type string :desc "Input X"))
               :output-fields (list '(:name y :type string :desc "Output Y" :prefix "Y:"))))
         (cot (dsel-make-chain-of-thought sig :lm dsel-test-llm-provider
                                          :rationale-field-prefix "Rationale:"))
         ;; Define the expected response for this specific test case via the map
         (dsel-test-llm-prompt-to-response-map
          `((,(dsel--format-input-fields (dsel-predict-signature cot) '((x . "value"))) .
             "Rationale: Custom rationale for COT test.\n\nY: custom_y_value\n\n")))
         (expected-raw-response "Rationale: Custom rationale for COT test.\n\nY: custom_y_value\n\n")
         (prediction (dsel-forward cot :x "value")))

    (should (dsel-prediction-p prediction))
    (should (equal (dsel-prediction-lm-provider prediction)
                   dsel-test-llm-provider))
    (should (equal (dsel-prediction-raw-response prediction)
                   expected-raw-response))
    (should (string= (dsel-get-field prediction 'rationale) "Custom rationale for COT test."))
    (should (string= (dsel-get-field prediction 'y) "custom_y_value"))))

;; Test chain-of-thought with demos (already fine as it doesn't rely on default response for assertions)
(ert-deftest dsel-test-chain-of-thought-demos ()
  "Test that demos are properly passed to the chain-of-thought predictor."
  (let* ((sig (dsel-make-signature
               "Test COT with Demos"
               :name 'test-cot-demos
               :input-fields (list '(:name input :type string :desc "Input Description")) ; Added desc
               :output-fields (list '(:name output :type string :desc "Output Description")))) ; Added desc
         (demo1 (dsel-make-example :input "sample1" :output "result1"))
         (demo2 (dsel-make-example :input "sample2" :output "result2"))
         (demos (list demo1 demo2))
         (cot (dsel-make-chain-of-thought sig
                                          :lm dsel-test-llm-provider
                                          :demos demos))
         )
    (should (= (length (dsel-predict-demos cot)) 2))
    (should (equal (dsel-predict-demos cot) demos))
    (let ((first-demo (car (dsel-predict-demos cot))))
      (should (string= (dsel-get-field first-demo 'input) "sample1"))
      (should (string= (dsel-get-field first-demo 'output) "result1")))
    (let ((second-demo (cadr (dsel-predict-demos cot))))
      (should (string= (dsel-get-field second-demo 'input) "sample2"))
      (should (string= (dsel-get-field second-demo 'output) "result2")))))

;; Test tracing functionality
(ert-deftest dsel-test-predict-tracing ()
  "Test that predictions are added to the trace when tracing is enabled."
  (let* ((sig (dsel-make-signature
               "Test tracing"
               :name 'test-trace-signature
               :input-fields (list '(:name input :type string :desc "Input value"))
               :output-fields (list '(:name output :type string :desc "Output value" :prefix "Output:"))))
         (predictor (dsel-make-predict sig :lm dsel-test-llm-provider))
         (dsel-test-llm-prompt-to-response-map
          `((,(dsel--format-input-fields sig '((input . "test"))) . "Output:traced\n\n"))))

    ;; Define a variable for tracing
    (defvar my-trace-list nil)
    (setq my-trace-list nil)

    ;; Test with tracing disabled
    (let ((dsel-settings--trace nil))
      (dsel-forward predictor :input "test")
      (should (null my-trace-list)))

    ;; Test with tracing enabled
    (let ((dsel-settings--trace 'my-trace-list))
      (dsel-forward predictor :input "test")
      (should (= (length my-trace-list) 1))
      (let ((trace-entry (car my-trace-list)))
        (should (= (length trace-entry) 3))
        (should (eq (car trace-entry) predictor))
        (should (equal (cadr trace-entry) '((input . "test"))))
        (should (dsel-prediction-p (caddr trace-entry)))
        (should (equal (dsel-get-field (caddr trace-entry) 'output) "traced"))))))

(provide 'dsel-predictors-tests)
;;; dsel-predictors-tests.el ends here
