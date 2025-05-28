;;; dsel-signature-tests.el --- Tests for JSON Schema-style fields  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains tests for the JSON Schema-style field definitions,
;; where fields are now plists with :name, :type, etc. instead of an alist.

;;; Code:

(require 'ert)
(require 'dsel)

;;; Basic Field Structure Tests

(ert-deftest dsel-test-json-schema-basic-structure ()
  "Test basic structure of JSON Schema-style field definitions."
  (let* ((sig (dsel-make-signature
               "Test JSON Schema fields"
               :input-fields
               (list
                '(:name text
                        :type string
                        :desc "The input text"))
               :output-fields
               (list
                '(:name sentiment
                        :type string
                        :desc "The sentiment: positive, negative, or neutral")))))

    ;; Test structure
    (should (dsel-signature-p sig))
    (should (listp (dsel-signature-input-fields sig)))
    (should (listp (dsel-signature-output-fields sig)))
    
    ;; Test field access
    (let ((text-field (dsel-signature-get-input-field sig 'text))
          (sentiment-field (dsel-signature-get-output-field sig 'sentiment)))
      (should text-field)
      (should sentiment-field)
      (should (eq 'text (dsel-field-name text-field)))
      (should (eq 'string (dsel-field-type text-field)))
      (should (eq 'sentiment (dsel-field-name sentiment-field)))
      (should (eq 'string (dsel-field-type sentiment-field)))
      
      ;; Test default prefix generation
      (should (string= "Text:" (dsel-field-prefix text-field)))
      (should (string= "Sentiment:" (dsel-field-prefix sentiment-field))))))

(ert-deftest dsel-test-json-schema-field-names ()
  "Test field name extraction functions."
  (let* ((sig (dsel-make-signature
               "Test field names"
               :input-fields
               (list
                '(:name input1 :type string :desc "Input 1")
                '(:name input2 :type string :desc "Input 2"))
               :output-fields
               (list
                '(:name output1 :type string :desc "Output 1")
                '(:name output2 :type string :desc "Output 2")))))
    
    ;; Test field name extraction
    (should (equal '(input1 input2) (dsel-signature-input-field-names sig)))
    (should (equal '(output1 output2) (dsel-signature-output-field-names sig)))))

;;; Complex Type Tests

(ert-deftest dsel-test-json-schema-complex-types ()
  "Test JSON Schema-style complex field types."
  (let* ((sig (dsel-make-signature 
               "Test complex types"
               :input-fields
               (list
                '(:name query
                        :type string
                        :desc "Search query"))
               :output-fields
               (list
                '(:name results
                        :type array
                        :desc "Search results"
                        :items (:type object
                                      :properties
                                      ((:name title
                                              :type string
                                              :desc "Result title")
                                       (:name url
                                              :type string
                                              :desc "Result URL")
                                       (:name tags
                                              :type array
                                              :desc "Result tags"
                                              :items (:type string)))))))))

    ;; Test nested types
    (let* ((results-field (dsel-signature-get-output-field sig 'results))
           (items (dsel-field-items results-field))
           (properties (dsel-field-properties items))
           (title-field (dsel-get-field-by-name properties 'title))
           (tags-field (dsel-get-field-by-name properties 'tags)))
      
      (should (eq 'array (dsel-field-type results-field)))
      (should (eq 'object (dsel-field-type items)))
      (should (eq 'string (dsel-field-type title-field)))
      (should (eq 'array (dsel-field-type tags-field)))
      (should (eq 'string (dsel-field-type (dsel-field-items tags-field)))))))

;;; Validation Tests

(ert-deftest dsel-test-json-schema-validation ()
  "Test validation of JSON Schema-style fields."
  ;; Missing :name
  (should-error
   (dsel-make-signature
    "Test validation"
    :input-fields
    (list
     '(:type string
             :desc "Field without name")))
   :type 'error)
  
  ;; Non-symbol :name
  (should-error
   (dsel-make-signature
    "Test validation"
    :input-fields
    (list
     '(:name "string-name"
             :type string
             :desc "Field with string name")))
   :type 'error)
  
  ;; Missing :type
  (should-error
   (dsel-make-signature
    "Test validation"
    :input-fields
    (list
     '(:name field
             :desc "Field without type")))
   :type 'error)
  
  ;; :desc is now optional, so this should NOT error.
  ;; Instead, we can check that it defaults to "".
  (let ((sig (dsel-make-signature "Test field without desc"
                                  :input-fields (list '(:name field :type string)))))
    (should (string= (dsel-field-desc (dsel-signature-get-input-field sig 'field)) "")))

  ;; Array without items
  (should-error
   (dsel-make-signature
    "Test validation"
    :input-fields
    (list
     '(:name tags
             :type array
             :desc "Tags without items")))
   :type 'error)
  
  ;; Object without properties
  (should-error
   (dsel-make-signature
    "Test validation"
    :input-fields
    (list
     '(:name user
             :type object
             :desc "User without properties")))
   :type 'error))

(provide 'dsel-signature-tests)
;;; dsel-signature-tests.el ends here
