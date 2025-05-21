;;; dsel-complex-types-tests.el --- Tests for complex field types in DSEL  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains tests for complex field types in the DSEL framework,
;; including testing nested objects, arrays, complex combinations, and
;; validation behavior.

;;; Code:

(require 'ert)
(require 'dsel)

;;; Tests for Array Field Types

(ert-deftest dsel-test-array-of-objects ()
  "Test array of objects field type."
  (let* ((sig (dsel-make-signature
               "Test array of objects"
               :input-fields (list '(:name simple-data :type string :desc "Simple text input"))
               :output-fields (list '(:name users
                                            :type array
                                            :desc "List of user objects"
                                            :items (:type object
                                                          :properties ((:name name :type string :desc "User name")
                                                                       (:name age :type integer :desc "User age"))))))))
    (let ((users-field (dsel-signature-get-output-field sig 'users)))
      (should (eq 'array (plist-get users-field :type)))
      (let ((items (plist-get users-field :items)))
        (should (eq 'object (plist-get items :type)))
        (let ((props (plist-get items :properties)))
          (should (dsel-get-field-by-name props 'name))
          (should (dsel-get-field-by-name props 'age)))))))

(ert-deftest dsel-test-array-validation ()
  "Test validation of array fields."
  ;; Arrays require :items
  (should-error
   (dsel-make-signature
    "Test array validation"
    :input-fields (list '(:name tags :type array :desc "Tags without items")))
   :type 'error)

  ;; Items requires :type
  (should-error
   (dsel-make-signature
    "Test array items validation"
    :input-fields (list '(:name data
                                :type array
                                :desc "Data array"
                                :items ())))
   :type 'error))

(ert-deftest dsel-test-nested-arrays ()
  "Test deeply nested array structures."
  (let* ((sig (dsel-make-signature
               "Test nested arrays"
               :output-fields
               (list '(:name matrix
                             :type array
                             :desc "3D matrix of numbers"
                             :items (:type array
                                           :items (:type array
                                                         :items (:type number))))))))
    (let* ((matrix-field (dsel-signature-get-output-field sig 'matrix))
           (level1-items (plist-get matrix-field :items))
           (level2-items (plist-get level1-items :items))
           (level3-items (plist-get level2-items :items)))
      (should (eq 'array (plist-get matrix-field :type)))
      (should (eq 'array (plist-get level1-items :type)))
      (should (eq 'array (plist-get level2-items :type)))
      (should (eq 'number (plist-get level3-items :type))))))

;;; Tests for Object Field Types

(ert-deftest dsel-test-deep-nested-objects ()
  "Test deeply nested object structures."
  (let* ((sig (dsel-make-signature
               "Test nested objects"
               :output-fields
               (list '(:name user
                             :type object
                             :desc "User record"
                             :properties ((:name name :type string :desc "Full name")
                                          (:name contact
                                                 :type object
                                                 :desc "Contact information"
                                                 :properties ((:name email :type string :desc "Email address")
                                                              (:name phone :type string :desc "Phone number")
                                                              (:name address
                                                                     :type object
                                                                     :desc "Physical address"
                                                                     :properties ((:name street :type string :desc "Street")
                                                                                  (:name city :type string :desc "City")
                                                                                  (:name country :type string :desc "Country")))))
                                          (:name stats
                                                 :type object
                                                 :desc "User statistics"
                                                 :properties ((:name joined :type string :desc "Join date")
                                                              (:name last-login :type string :desc "Last login date"))))
                             :required (name))))))
    (let* ((user-field (dsel-signature-get-output-field sig 'user))
           (properties (plist-get user-field :properties))
           (contact-field (dsel-get-field-by-name properties 'contact))
           (contact-properties (plist-get contact-field :properties))
           (address-field (dsel-get-field-by-name contact-properties 'address))
           (address-properties (plist-get address-field :properties)))
      ;; Top level validation
      (should (eq 'object (plist-get user-field :type)))
      (should (equal '(name) (plist-get user-field :required)))

      ;; Level 2 validation
      (should (eq 'object (plist-get contact-field :type)))
      (should (dsel-get-field-by-name contact-properties 'email))
      (should (dsel-get-field-by-name contact-properties 'phone))

      ;; Level 3 validation
      (should (eq 'object (plist-get address-field :type)))
      (should (dsel-get-field-by-name address-properties 'street))
      (should (dsel-get-field-by-name address-properties 'city))
      (should (dsel-get-field-by-name address-properties 'country)))))

(ert-deftest dsel-test-object-validation ()
  "Test validation of object fields."
  ;; Objects require :properties
  (should-error
   (dsel-make-signature
    "Test object validation"
    :input-fields (list '(:name user :type object :desc "User data with no props"))) ; desc for parent
   :type 'error)

  ;; Properties can now omit :desc (it defaults to "")
  (let ((sig (dsel-make-signature
              "Test object properties validation with optional desc"
              :input-fields (list '(:name user
                                          :type object
                                          :desc "User data"
                                          :properties ((:name name :type string) ; :desc omitted for property 'name'
                                                       (:name age :type integer :desc "User age")))))))
    (let* ((user-field (dsel-signature-get-input-field sig 'user))
           (props (plist-get user-field :properties))
           (name-prop (dsel-get-field-by-name props 'name)))
      (should (string= (plist-get name-prop :desc) "")))) ; Verify desc defaults to ""

  ;; Test that a property missing :type still errors
  (should-error
   (dsel-make-signature
    "Test object property missing type"
    :input-fields (list '(:name user
                                :type object
                                :desc "User data with property missing type"
                                :properties ((:name email :desc "User email")))))
   :type 'error))

;;; Tests for Enum Field Types

(ert-deftest dsel-test-enum-with-defaults ()
  "Test enum fields with default values."
  (let* ((sig (dsel-make-signature
               "Test enum with defaults"
               :input-fields (list '(:name status
                                           :type string
                                           :desc "Current status"
                                           :enum ["active" "inactive" "pending"]
                                           :optional t)))))
    (let ((status-field (dsel-signature-get-input-field sig 'status)))
      (should (eq 'string (plist-get status-field :type)))
      (should (plist-get status-field :optional))
      (should (equal ["active" "inactive" "pending"] (plist-get status-field :enum))))))

(ert-deftest dsel-test-numeric-enums ()
  "Test enums with numeric values."
  (let* ((sig (dsel-make-signature
               "Test numeric enums"
               :input-fields (list '(:name priority-level
                                           :type integer
                                           :desc "Priority level"
                                           :enum [1 2 3 4 5])))))
    (let ((priority-field (dsel-signature-get-input-field sig 'priority-level)))
      (should (eq 'integer (plist-get priority-field :type)))
      (should (equal [1 2 3 4 5] (plist-get priority-field :enum))))))


;;; Tests for Field Formatting

(ert-deftest dsel-test-field-formatting ()
  "Test that field names are properly formatted into prefixes."
  (let* ((sig (dsel-make-signature
               "Test field formatting"
               :input-fields (list '(:name simple-name :type string :desc "Simple kebab case")
                                   '(:name snake_case_name :type string :desc "Snake case name")
                                   '(:name camelCaseName :type string :desc "Camel case name")
                                   '(:name PascalCaseName :type string :desc "Pascal case name")
                                   '(:name UPPERCASE_NAME :type string :desc "Uppercase name")))))
    ;; Check all field prefixes are capitalized properly
    (let ((fields (dsel-signature-input-fields sig)))
      (let ((simple-field (dsel-get-field-by-name fields 'simple-name)))
        (should (string= "Simple-Name:" (plist-get simple-field :prefix))))

      (let ((snake-field (dsel-get-field-by-name fields 'snake_case_name)))
        (should (string= "Snake_Case_Name:" (plist-get snake-field :prefix))))

      (let ((camel-field (dsel-get-field-by-name fields 'camelCaseName)))
        (should (string= "Camelcasename:" (plist-get camel-field :prefix))))

      (let ((pascal-field (dsel-get-field-by-name fields 'PascalCaseName)))
        (should (string= "Pascalcasename:" (plist-get pascal-field :prefix))))

      (let ((upper-field (dsel-get-field-by-name fields 'UPPERCASE_NAME)))
        (should (string= "Uppercase_Name:" (plist-get upper-field :prefix)))))))

;;; Integration Tests for End-to-End Flow

(ert-deftest dsel-test-integration-complex-signature ()
  "Test end-to-end flow with a complex signature including nested objects and arrays."
  (let* ((sig (dsel-make-signature
               "Analyze product data with nested structure"
               :input-fields (list '(:name query :type string :desc "Search query"))
               :output-fields
               (list '(:name product
                             :type object
                             :desc "Product details"
                             :properties ((:name name :type string :desc "Product name")
                                          (:name price :type number :desc "Product price")
                                          (:name available :type boolean :desc "Is available")
                                          (:name categories
                                                 :type array
                                                 :desc "Product categories"
                                                 :items (:type string))
                                          (:name metadata
                                                 :type object
                                                 :desc "Additional metadata"
                                                 :properties ((:name created_at :type string :desc "Creation date")
                                                              (:name rating :type number :desc "Product rating")
                                                              (:name tags
                                                                     :type array
                                                                     :desc "Product tags"
                                                                     :items (:type string)))))))))
         ;; Create predictor
         (predictor (dsel-make-predict sig :lm dsel-test-llm-provider))

         ;; Mock the LLM response with a complex structure
         ;; Note: Using format-input-fields to generate the correct key for our mock response
         (mock-response "Product:
{
  \"name\": \"Premium Coffee Maker\",
  \"price\": 129.99,
  \"available\": true,
  \"categories\": [\"Kitchen\", \"Appliances\", \"Coffee\"],
  \"metadata\": {
    \"created_at\": \"2025-03-15\",
    \"rating\": 4.8,
    \"tags\": [\"best-seller\", \"premium\", \"stainless-steel\"]
  }
}")
         (dsel-test-llm-prompt-to-response-map
          `((,(dsel--format-input-fields sig '((query . "coffee maker"))) . ,mock-response)))

         ;; Run the prediction
         (prediction (dsel-forward predictor :query "coffee maker")))

    ;; Test the prediction result
    (should (dsel-prediction-p prediction))

    ;; Test input is preserved
    (should (string= (dsel-get-field prediction 'query) "coffee maker"))

    ;; Access top-level product object
    (let ((product (dsel-get-field prediction 'product)))
      (should (listp product))

      ;; Test basic fields
      (should (string= (alist-get 'name product) "Premium Coffee Maker"))
      (should (= (alist-get 'price product) 129.99))
      (should (eq (alist-get 'available product) t))

      ;; Test array field
      (let ((categories (alist-get 'categories product)))
        (should (vectorp categories))
        (should (= (length categories) 3))
        (should (string= (aref categories 0) "Kitchen"))
        (should (string= (aref categories 1) "Appliances"))
        (should (string= (aref categories 2) "Coffee")))

      ;; Test nested object
      (let ((metadata (alist-get 'metadata product)))
        (should (listp metadata))
        (should (string= (alist-get 'created_at metadata) "2025-03-15"))
        (should (= (alist-get 'rating metadata) 4.8))

        ;; Test nested array in object
        (let ((tags (alist-get 'tags metadata)))
          (should (vectorp tags))
          (should (= (length tags) 3))
          (should (string= (aref tags 0) "best-seller"))
          (should (string= (aref tags 1) "premium"))
          (should (string= (aref tags 2) "stainless-steel")))))))

(provide 'dsel-complex-types-tests)
;;; dsel-complex-types-tests.el ends here
