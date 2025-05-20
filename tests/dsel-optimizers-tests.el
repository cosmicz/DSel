;;; dsel-optimizers-tests.el --- Tests for dsel optimizers  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ERT tests for the dsel-optimizers functionality, including
;; labeled-fewshot and bootstrap-fewshot.

;;; Code:

(require 'ert)
(require 'dsel)

;; Test labeled-fewshot optimizer
(ert-deftest dsel-test-labeled-fewshot ()
  "Test `dsel-make-labeled-fewshot` and compile behavior."
  (let* ((sig (dsel-make-signature
               "Test LF"
               :name 'lf-sig
               :input-fields '((a . (:type string :desc "Input A")))
               :output-fields '((b . (:type string :desc "Output B")))))
         (student (dsel-make-predict sig))
         (examples (list
                    (dsel-make-example :a "one" :b "1")
                    (dsel-make-example :a "two" :b "2")))
         (optimizer (dsel-make-labeled-fewshot :k 1))
         (compiled (dsel-compile optimizer student :trainset examples)))
    (should (dsel-module-p compiled))
    (should (dsel-module-compiled-p compiled))
    ;; Should have only k labeled demos in predictors
    (let ((predictors (dsel-collect-predictors compiled)))
      (should (= (length predictors) 1))
      (let ((pred (car predictors)))
        (should (= (length (dsel-predict-demos pred)) 1))))))

;; Test bootstrap-fewshot optimizer
(ert-deftest dsel-test-bootstrap-fewshot ()
  "Test `dsel-make-bootstrap-fewshot` compile behavior."
  (let* ((sig (dsel-make-signature
               "Test BF"
               :name 'bf-sig
               :input-fields '((a . (:type string :desc "Input A")))
               :output-fields '((b . (:type string :desc "Output B")))))
         (student (dsel-make-predict sig))
         (examples (list
                    (dsel-example-with-inputs (dsel-make-example :a "one" :b "1") 'a)
                    (dsel-example-with-inputs (dsel-make-example :a "two" :b "2") 'a)
                    (dsel-example-with-inputs (dsel-make-example :a "three" :b "3") 'a)
                    ))
         (dummy-metric (lambda (gold pred)
                         (string= (dsel-example-field gold 'b)
                                  (dsel-example-field pred 'b))))
         (optimizer (dsel-make-bootstrap-fewshot
                     :metric dummy-metric
                     :k-labeled 1
                     :k-bootstrapped 1
                     ))
         (compiled
          (let ((dsel-test-llm-prompt-to-response-map ; fake llm behavior
                 '(("A: one" . "B: 1")
                   ("A: two" . "B: 2"))))
            ;; Use the same student as teacher for simplicity
            (dsel-compile optimizer student :trainset examples :teacher student))))
    (should (dsel-module-p compiled))
    (should (dsel-module-compiled-p compiled))
    ;; Should have at least k-labeled demos
    (let ((predictors (dsel-collect-predictors compiled)))
      (should (= (length predictors) 1))
      (let ((pred (car predictors)))
        (should (>= (length (dsel-predict-demos pred)) 2))
        (should (>= (length (dsel-predict-demos pred)) 1))))))

(provide 'dsel-optimizers-tests)
;;; dsel-optimizers-tests.el ends here
