;;; dsel-signature-extended-tests.el --- Tests for enhanced dsel-signature fields  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains ERT tests for testing enhanced dsel-signature
;; field types, including enums, arrays, objects, and other complex types.

;;; Code:

(require 'ert)
(require 'dsel)

(ert-deftest dsel-test-signature-basic-and-default-prefix ()
  "Test basic signature creation and default prefix generation."
  (let* ((instructions "Test instruction.")
         (sig (dsel-make-signature
               instructions
               :name 'test-sig
               :input-fields
               (list
                '(:name inField
                        :type string
                        :desc "Input field"))
               :output-fields
               (list
                '(:name outField
                        :type string
                        :desc "Output field")))))
    (should (dsel-signature-p sig))
    (should (eq 'test-sig (dsel-signature-name sig)))
    (should (string= instructions (dsel-signature-instructions sig)))
    (let ((in-field (dsel-signature-get-input-field sig 'inField))
          (out-field (dsel-signature-get-output-field sig 'outField)))
      (should (string= "Infield:" (dsel-field-prefix in-field)))
      (should (string= "Outfield:" (dsel-field-prefix out-field))))))

(ert-deftest dsel-test-signature-with-enum ()
  "Test signature creation with an enum field."
  (let* ((sig (dsel-make-signature
               "Select a color."
               :input-fields
               (list
                '(:name color-choice
                        :type string
                        :desc "Choose a color"
                        :enum ["red" "green" "blue"])))))
    (let ((field (dsel-signature-get-input-field sig 'color-choice)))
      (should (equal ["red" "green" "blue"] (dsel-field-enum field))))))

(ert-deftest dsel-test-signature-with-optional-field ()
  "Test signature creation with an optional field."
  (let* ((sig (dsel-make-signature
               "Describe item."
               :input-fields
               (list
                '(:name item-name
                        :type string
                        :desc "Name")
                '(:name item-details
                        :type string
                        :desc "Details"
                        :optional t)))))
    (let ((details-field (dsel-signature-get-input-field sig 'item-details)))
      (should (dsel-field-optional details-field)))))

(ert-deftest dsel-test-signature-with-array-of-strings ()
  "Test signature with an array of strings."
  (let* ((sig (dsel-make-signature
               "List tags."
               :output-fields
               (list
                '(:name tags
                        :type array
                        :desc "A list of tags"
                        :items (:type string))))))
    (let ((tags-field (dsel-signature-get-output-field sig 'tags)))
      (should (eq 'array (dsel-field-type tags-field)))
      (should (eq 'string (dsel-field-type (dsel-field-items tags-field)))))))

(ert-deftest dsel-test-signature-with-object ()
  "Test signature with an object field."
  (let* ((sig (dsel-make-signature
               "User details."
               :output-fields
               (list
                '(:name user
                        :type object
                        :desc "User object"
                        :properties ((:name name
                                            :type string
                                            :desc "User's name")
                                     (:name age
                                            :type integer
                                            :desc "User's age"
                                            :optional t))
                        :required (name))))))
    (let ((user-field (dsel-signature-get-output-field sig 'user)))
      (should (eq 'object (dsel-field-type user-field)))
      (let ((props (dsel-field-properties user-field)))
        (should (listp props))
        (let ((name-prop (dsel-get-field-by-name props 'name))
              (age-prop (dsel-get-field-by-name props 'age)))
          (should (eq 'string (dsel-field-type name-prop)))
          (should (string= "User's name" (dsel-field-desc name-prop)))
          (should (eq 'integer (dsel-field-type age-prop)))
          (should (dsel-field-optional age-prop))))
      (should (equal '(name) (dsel-field-required user-field))))))

(ert-deftest dsel-test-signature-missing-required-plist-keys ()
  "Test that `dsel-make-signature` errors if :type are missing (:desc is optional)."
  (let ((sig (dsel-make-signature "Test with no desc" :input-fields (list '(:name no-desc :type string)))))
    (should (string= (dsel-field-desc (dsel-signature-get-input-field sig 'no-desc)) "")))

  (should-error (dsel-make-signature "Test" :input-fields (list '(:name no-type :desc "test")))
                :type 'error))

(ert-deftest dsel-test-signature-array-requires-items ()
  "Test that array type fields require :items."
  (should-error
   (dsel-make-signature 
    "Test array validation"
    :input-fields (list '(:name tags :type array :desc "Tags without items")))
   :type 'error))

(ert-deftest dsel-test-signature-object-requires-properties ()
  "Test that object type fields require :properties."
  (should-error 
   (dsel-make-signature 
    "Test object validation"
    :input-fields (list '(:name user :type object :desc "User without properties")))
   :type 'error))

(ert-deftest dsel-test-signature-with-nested-array ()
  "Test signature with nested array field."
  (let* ((sig (dsel-make-signature
               "Matrix representation."
               :input-fields (list '(:name matrix 
                                           :type array
                                           :desc "A matrix of numbers"
                                           :items (:type array
                                                         :items (:type number)))))))
    (let* ((matrix-field (dsel-signature-get-input-field sig 'matrix))
           (items-plist (dsel-field-items matrix-field)))
      (should (eq 'array (dsel-field-type matrix-field)))
      (should (eq 'array (dsel-field-type items-plist)))
      (should (eq 'number (dsel-field-type (dsel-field-items items-plist)))))))

(ert-deftest dsel-test-signature-with-complex-object ()
  "Test signature with complex nested object field."
  (let* ((sig (dsel-make-signature
               "Product information."
               :output-fields
               (list '(:name product 
                             :type object
                             :desc "Product details"
                             :properties ((:name name :type string :desc "Product name")
                                          (:name price :type number :desc "Product price")
                                          (:name categories
                                                 :type array
                                                 :desc "Product categories"
                                                 :items (:type string))
                                          (:name metadata
                                                 :type object
                                                 :desc "Additional metadata"
                                                 :properties ((:name created-at :type string :desc "Creation date")
                                                              (:name updated-at :type string :desc "Last update date"))
                                                 :required (created-at)))
                             :required (name price))))))
    (let* ((product-field (dsel-signature-get-output-field sig 'product))
           (props (dsel-field-properties product-field))
           (categories-field (dsel-get-field-by-name props 'categories))
           (metadata-field (dsel-get-field-by-name props 'metadata)))
      
      ;; Check top-level object
      (should (eq 'object (dsel-field-type product-field)))
      (should (equal '(name price) (dsel-field-required product-field)))
      
      ;; Check array field
      (should (eq 'array (dsel-field-type categories-field)))
      (should (eq 'string (dsel-field-type (dsel-field-items categories-field))))
      
      ;; Check nested object
      (should (eq 'object (dsel-field-type metadata-field)))
      (let ((metadata-props (dsel-field-properties metadata-field)))
        (should (dsel-get-field-by-name metadata-props 'created-at)))
      (should (equal '(created-at) (dsel-field-required metadata-field))))))

(provide 'dsel-signature-extended-tests)
;;; dsel-signature-extended-tests.el ends here
