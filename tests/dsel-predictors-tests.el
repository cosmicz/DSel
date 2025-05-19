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

(provide 'dsel-predictors-tests)
;;; dsel-predictors-tests.el ends here
