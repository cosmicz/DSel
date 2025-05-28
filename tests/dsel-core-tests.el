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

;; Test dsel-defmodule

(ert-deftest dsel-test-defmodule ()
  "Test defining a custom module using dsel-defmodule."
  (require 'dsel-macros)

  ;; Define a simple test signature for use in the module
  (let ((test-signature (dsel-make-signature "Test"
                                             :name 'test-sig
                                             :input-fields (list (dsel-make-field :name 'input :type 'string))
                                             :output-fields (list (dsel-make-field :name 'output :type 'string)))))

    ;; Define a test module using dsel-defmodule with various slot formats
    (dsel-defmodule test-custom-module
      "Test module for DSel with custom docstring."
      :submodules ((predictor (dsel-make-predict test-signature))
                   (helper (make-dsel-module :name 'helper)))
      ;; Slots with various formats:
      config                        ; Simple slot without default
      (max-tokens 1000 :type number)  ; Slot with default and type
      (model-name "gpt-4" :type string)  ; String slot with default and type
      (temperature 0.5 :type float)  ; Float slot with default
      (options '(:key1 val1) :type list :read-only t))  ; Complex slot with options
    
    ;; Test that the module type and constructor are defined correctly
    (should (fboundp 'make-test-custom-module))
    (should (fboundp 'test-custom-module-p))
    
    ;; Create an instance with default values
    (let* ((default-module (make-test-custom-module :name 'default-instance)))
      
      ;; Test default values are correctly initialized
      (should (test-custom-module-p default-module))
      (should (null (test-custom-module-config default-module)))
      (should (= (test-custom-module-max-tokens default-module) 1000))
      (should (string= (test-custom-module-model-name default-module) "gpt-4"))
      (should (= (test-custom-module-temperature default-module) 0.5))
      (should (equal (test-custom-module-options default-module) '(:key1 val1)))
      
      ;; Test helper submodule exists with default value
      (let ((helper (dsel-module-get-submodule default-module 'helper)))
        (should helper)
        (should (dsel-module-p helper))))
    
    ;; Create an instance with custom values
    (let* ((predictor (dsel-make-predict test-signature :name 'custom-predictor))
           (custom-module (make-test-custom-module
                           :name 'custom-instance
                           :predictor predictor
                           :max-tokens 2000
                           :model-name "gemma"
                           :temperature 0.8
                           :config '(:beam-width 4))))

      ;; Test structure and inheritance
      (should (test-custom-module-p custom-module))
      (should (dsel-module-p custom-module))
      
      ;; Test custom values overrode defaults
      (should (eq (dsel-module-name custom-module) 'custom-instance))
      (should (= (test-custom-module-max-tokens custom-module) 2000))
      (should (string= (test-custom-module-model-name custom-module) "gemma"))
      (should (= (test-custom-module-temperature custom-module) 0.8))
      (should (equal (test-custom-module-config custom-module) '(:beam-width 4)))
      (should (equal (test-custom-module-options custom-module) '(:key1 val1)))  ; Read-only, shouldn't change
      
      ;; Test submodules
      (should (eq (dsel-module-get-submodule custom-module 'predictor) predictor))
      (let ((helper (dsel-module-get-submodule custom-module 'helper)))
        (should helper)
        (should (dsel-module-p helper))))))

(provide 'dsel-core-tests)
;;; dsel-core-tests.el ends here
