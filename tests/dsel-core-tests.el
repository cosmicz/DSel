;;; dsel-core-tests.el --- Tests for dsel core functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains ERT tests for the dsel core functionality.

;;; Code:

(require 'ert)
(require 'dsel)

;; Test dsel-signature

(ert-deftest dsel-test-signature ()
  "Test creation and behavior of dsel-signature."
  (let* ((instructions "Classify the sentiment of the text.")
         (sig (dsel-make-signature
               instructions
               :name 'sentiment-classifier
               :input-fields (list '(:name text :type string :desc "The text to classify"))
               :output-fields (list '(:name sentiment :type string :desc "The sentiment: positive, negative, or neutral")))))
    
    ;; Test structure
    (should (dsel-signature-p sig))
    (should (eq (dsel-signature-name sig) 'sentiment-classifier))
    (should (string= (dsel-signature-instructions sig) instructions))
    
    ;; Test field access
    (should (= (length (dsel-signature-input-fields sig)) 1))
    (should (= (length (dsel-signature-output-fields sig)) 1))
    
    ;; Test default prefix generation
    (let ((text-field (dsel-signature-get-input-field sig 'text))
          (sentiment-field (dsel-signature-get-output-field sig 'sentiment)))
      (should (string= (dsel-field-prefix text-field) "Text:"))
      (should (string= (dsel-field-prefix sentiment-field) "Sentiment:")))))

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

;; Test dsel-module-get-submodule

(ert-deftest dsel-test-module-get-submodule ()
  "Test getting a submodule by name."
  (let* ((module (make-dsel-module :name 'parent-module))
         (child1 (make-dsel-module :name 'child1))
         (child2 (make-dsel-module :name 'child2))
         (submodules (list (cons 'child1 child1)
                           (cons 'child2 child2))))

    ;; Set up module with submodules
    (setf (dsel-module-submodules module) submodules)

    ;; Test retrieving submodules by name
    (should (eq (dsel-module-get-submodule module 'child1) child1))
    (should (eq (dsel-module-get-submodule module 'child2) child2))
    (should (eq (dsel-module-get-submodule module 'non-existent) nil))))

(provide 'dsel-core-tests)
;;; dsel-core-tests.el ends here
