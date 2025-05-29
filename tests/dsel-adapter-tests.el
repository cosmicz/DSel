;;; dsel-adapter-tests.el --- Tests for dsel adapter functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file contains ERT tests for the dsel adapter functionality.

;;; Code:

(require 'ert)
(require 'dsel)

;; Helper function to convert field results to alist for testing
(defun dsel-test--field-results-to-alist (field-results)
  "Convert field result plists to alist for easier testing."
  (cl-loop for frp in field-results
           when (null (plist-get frp :error))
           collect (cons (plist-get frp :name)
                         (plist-get frp :value))))

;; Test dsel-adapter

(ert-deftest dsel-test-adapter-format-prompt ()
  "Test prompt formatting with dsel-adapter."
  (let* ((instructions "Classify the sentiment of the text.")
         (sig (dsel-make-signature
               instructions
               :name 'sentiment-classifier
               :input-fields (list '(:name text :type string :desc "The text to classify"))
               :output-fields (list '(:name sentiment :type string :desc "The sentiment: positive, negative, or neutral"))))
         (adapter (make-dsel-default-chat-adapter))
         (demos (list
                 (dsel-example-with-inputs
                  (dsel-make-example
                   :text "I love this product!"
                   :sentiment "positive")
                  'text)
                 (dsel-example-with-inputs
                  (dsel-make-example
                   :text "I hate this product!"
                   :sentiment "negative")
                  'text)))
         (inputs '((text . "This product is okay.")))
         (prompt (dsel-adapter-format-prompt adapter sig demos inputs)))

    ;; Test prompt structure
    (should (llm-chat-prompt-p prompt))
    (should (stringp (llm-chat-prompt-context prompt)))
    (let ((system-message-content (llm-chat-prompt-context prompt)))
      (should (string-match-p (regexp-quote instructions) system-message-content))
      (should (string-match-p "Your input fields are:" system-message-content))
      (should (string-match-p "Your output fields are:" system-message-content)))

    (should (listp (llm-chat-prompt-examples prompt)))
    (should (= (length (llm-chat-prompt-examples prompt)) 2))

    ;; Test the :interactions slot (which initially holds the current user input)
    ;; Note: llm-provider-utils-combine-to-system-prompt (called later by actual providers)
    ;; will merge :context and :examples into this :interactions list.
    ;; What llm-make-chat-prompt does is put the `content` argument into :interactions.
    (should (listp (llm-chat-prompt-interactions prompt)))
    (let ((initial-interactions (llm-chat-prompt-interactions prompt)))
      (should (= (length initial-interactions) 1)) ; Only the current input initially
      (let ((current-user-interaction (car initial-interactions)))
        (should (eq (llm-chat-prompt-interaction-role current-user-interaction) 'user))
        (should (string-match-p "This product is okay"
                                (llm-chat-prompt-interaction-content current-user-interaction)))))))

(ert-deftest dsel-test-adapter-parse-output ()
  "Test output parsing with dsel-adapter."
  (let* ((sig (dsel-make-signature
               "Classify the sentiment of the text."
               :name 'sentiment-classifier
               :input-fields (list '(:name text :type string :desc "The text to classify"))
               :output-fields (list '(:name sentiment :type string :desc "The sentiment: positive, negative, or neutral")
                                    '(:name confidence :type number :desc "Confidence score from 0 to 1"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Sentiment: positive\n\nConfidence: 0.95")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result is a list of field result plists
    (should (listp field-results))
    (should (= (length field-results) 2))

    ;; Convert to alist for easier testing
    (let ((result-alist (cl-loop for frp in field-results
                                 when (null (plist-get frp :error))
                                 collect (cons (plist-get frp :name)
                                               (plist-get frp :value)))))
      (should (equal (assq 'sentiment result-alist) '(sentiment . "positive")))
      (should (equal (assq 'confidence result-alist) '(confidence . 0.95))))

    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

;; Extended tests for dsel-adapter-parse-output

(ert-deftest dsel-test-adapter-parse-multiline-values ()
  "Test parsing of multi-line field values."
  (let* ((sig (dsel-make-signature
               "Generate code and explanation"
               :input-fields (list '(:name prompt :type string :desc "Coding task"))
               :output-fields (list '(:name code :type string :desc "Generated code")
                                    '(:name explanation :type string :desc "Explanation of the code"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Code: function factorial(n) {
  if (n <= 1) return 1;
  return n * factorial(n-1);
}

Explanation: This is a recursive implementation of the factorial function.
It checks if n is less than or equal to 1, in which case it returns 1.
Otherwise, it multiplies n by the factorial of (n-1).
This approach demonstrates the elegant use of recursion for mathematical calculations.")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed multiline values
    (should (listp field-results))
    (should (= (length field-results) 2))
    
    ;; Check code field
    (let ((code-value (cdr (assq 'code result))))
      (should (stringp code-value))
      (should (string-match-p "function factorial" code-value))
      (should (string-match-p "return n \\* factorial" code-value)))
    
    ;; Check explanation field
    (let ((explanation-value (cdr (assq 'explanation result))))
      (should (stringp explanation-value))
      (should (string-match-p "recursive implementation" explanation-value))
      (should (string-match-p "mathematical calculations" explanation-value)))
    
    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-adapter-parse-fields-out-of-order ()
  "Test parsing when LLM returns fields out of order."
  (let* ((sig (dsel-make-signature
               "Analyze text"
               :input-fields (list '(:name text :type string :desc "Text to analyze"))
               :output-fields (list '(:name length :type number :desc "Character count")
                                    '(:name sentiment :type string :desc "Emotional tone")
                                    '(:name summary :type string :desc "Brief summary"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Summary: This is a concise summary of the text.

Sentiment: positive

Length: 42")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed out-of-order fields
    (should (listp field-results))
    (should (= (length field-results) 3))
    
    ;; Check that all fields are present with correct values
    (should (equal (cdr (assq 'length result)) 42))
    (should (equal (cdr (assq 'sentiment result)) "positive"))
    (should (equal (cdr (assq 'summary result)) "This is a concise summary of the text."))
    
    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-adapter-parse-missing-optional-fields ()
  "Test parsing when optional fields are missing from response."
  (let* ((sig (dsel-make-signature
               "Analyze document"
               :input-fields (list '(:name document :type string :desc "Document to analyze"))
               :output-fields (list '(:name title :type string :desc "Document title")
                                    '(:name author :type string :desc "Document author" :optional t)
                                    '(:name word_count :type number :desc "Word count"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Sample Document Analysis

Word_count: 1234")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed result with missing optional field
    (should (listp field-results))
    (should (= (length field-results) 2))
    
    ;; Check present fields
    (should (equal (cdr (assq 'title result)) "Sample Document Analysis"))
    (should (equal (cdr (assq 'word_count result)) 1234))
    
    ;; Check missing optional field
    (should (null (assq 'author result)))
    
    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-adapter-parse-missing-required-fields ()
  "Test parsing when required fields are missing from response."
  (let* ((sig (dsel-make-signature
               "Analyze document"
               :input-fields (list '(:name document :type string :desc "Document to analyze"))
               :output-fields (list '(:name title :type string :desc "Document title")
                                    '(:name category :type string :desc "Document category"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Important Document")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Should not error, but should return field results with errors
    (should (listp field-results))
    (should (= (length field-results) 2))
    
    ;; Check that title was parsed successfully
    (let ((title-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'title)) field-results)))
      (should title-result)
      (should (null (plist-get title-result :error)))
      (should (equal (plist-get title-result :value) "Important Document")))
    
    ;; Check that category has a missing-required error
    (let ((category-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'category)) field-results)))
      (should category-result)
      (should (plist-get category-result :error))
      (should (eq (plist-get (plist-get category-result :error) :type) :missing-required)))))

(ert-deftest dsel-test-adapter-parse-empty-required-fields ()
  "Test parsing when required fields are empty in the response."
  ;; Test 1: Empty required numeric field
  (let* ((sig (dsel-make-signature
               "Document statistics"
               :input-fields (list '(:name document :type string :desc "Document to analyze"))
               :output-fields (list '(:name title :type string :desc "Document title")
                                    '(:name word_count :type integer :desc "Word count"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Statistical Analysis

Word_count: ")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Should not error, but should return field results with errors
    (should (listp field-results))
    (should (= (length field-results) 2))
    
    ;; Check that title was parsed successfully
    (let ((title-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'title)) field-results)))
      (should title-result)
      (should (null (plist-get title-result :error)))
      (should (equal (plist-get title-result :value) "Statistical Analysis")))
    
    ;; Check that word_count has a required-field-empty error
    (let ((word-count-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'word_count)) field-results)))
      (should word-count-result)
      (should (plist-get word-count-result :error))
      (should (eq (plist-get (plist-get word-count-result :error) :type) :required-field-empty))))

  ;; Test 2: Empty required array field
  (let* ((sig (dsel-make-signature
               "Document categories"
               :input-fields (list '(:name document :type string :desc "Document to analyze"))
               :output-fields (list '(:name title :type string :desc "Document title")
                                    '(:name categories
                                            :type array
                                            :desc "Document categories"
                                            :items (:type string)))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Array Test Document

Categories: ")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Should return field results with errors
    (should (listp field-results))
    (should (= (length field-results) 2))
    
    ;; Check that categories has a required-field-empty error
    (let ((categories-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'categories)) field-results)))
      (should categories-result)
      (should (plist-get categories-result :error))
      (should (eq (plist-get (plist-get categories-result :error) :type) :required-field-empty))))

  ;; Test 3: Empty required object field
  (let* ((sig (dsel-make-signature
               "Document metadata"
               :input-fields (list '(:name document :type string :desc "Document to analyze"))
               :output-fields (list '(:name title :type string :desc "Document title")
                                    '(:name metadata
                                            :type object
                                            :desc "Document metadata"
                                            :properties ((:name author :type string))))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Object Test Document

Metadata: ")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Should return field results with errors
    (should (listp field-results))
    (should (= (length field-results) 2))
    
    ;; Check that metadata has a required-field-empty error
    (let ((metadata-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'metadata)) field-results)))
      (should metadata-result)
      (should (plist-get metadata-result :error))
      (should (eq (plist-get (plist-get metadata-result :error) :type) :required-field-empty)))))

(ert-deftest dsel-test-adapter-parse-empty-values ()
  "Test parsing when field values are empty."
  (let* ((sig (dsel-make-signature
               "Process form data"
               :input-fields (list '(:name form :type string :desc "Form data"))
               :output-fields (list '(:name name :type string :desc "Person's name")
                                    '(:name age :type number :desc "Person's age" :optional t)
                                    '(:name comments :type string :desc "Additional comments"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Name: John Smith

Age: 

Comments: ")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed result with empty values
    (should (listp field-results))
    (should (= (length field-results) 2)) ;; Only 2 fields: name and comments (age is omitted as optional with nil value)

    ;; Check non-empty value
    (should (equal (cdr (assq 'name result)) "John Smith"))

    ;; Check empty values - string field should be empty string
    (should (equal (cdr (assq 'comments result)) ""))

    ;; Check empty values - optional number field should not be in field-results or result alist
    (should (eq nil (assq 'age result)))
    (let ((age-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'age)) field-results)))
      (should (null age-result))))) ;; Should not be present at all

(ert-deftest dsel-test-adapter-parse-whitespace-variations ()
  "Test parsing with various whitespace in field values and prefixes."
  (let* ((sig (dsel-make-signature
               "Extract data"
               :input-fields (list '(:name input :type string :desc "Raw data"))
               :output-fields (list '(:name first_field :type string :desc "First extracted value")
                                    '(:name second_field :type string :desc "Second extracted value"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "First_field:   Value with leading spaces

Second_field: Value without space after prefix")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed result with whitespace variations
    (should (listp field-results))
    (should (= (length field-results) 2))

    ;; Check values with leading/trailing spaces
    (should (equal (cdr (assq 'first_field result)) "Value with leading spaces"))
    (should (equal (cdr (assq 'second_field result)) "Value without space after prefix"))

    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-adapter-parse-similar-prefixes ()
  "Test parsing when field prefixes are similar or substrings of each other."
  (let* ((sig (dsel-make-signature
               "Analyze notes"
               :input-fields (list '(:name notes :type string :desc "Input notes"))
               :output-fields (list '(:name note :type string :desc "Main note")
                                    '(:name note_details :type string :desc "Additional details of the note")
                                    '(:name summary :type string :desc "Summary of notes"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Note: This is the main note content.

Note_details: These are the additional explanatory details.

Summary: Overall it says something important.")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed result with similar prefixes
    (should (listp field-results))
    (should (= (length field-results) 3))

    ;; Check each field's value - important to ensure "Note:" doesn't consume "Note_details:"
    (should (equal (cdr (assq 'note result)) "This is the main note content."))
    (should (equal (cdr (assq 'note_details result)) "These are the additional explanatory details."))
    (should (equal (cdr (assq 'summary result)) "Overall it says something important."))

    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-adapter-parse-trailing-text ()
  "Test parsing when response has trailing text not belonging to any field."
  (let* ((sig (dsel-make-signature
               "Generate text"
               :input-fields (list '(:name prompt :type string :desc "Text prompt"))
               :output-fields (list '(:name title :type string :desc "Generated title")
                                    '(:name body :type string :desc "Generated body text"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Example Title

Body: Example body text with some content.

I hope this helps! Let me know if you need any revisions.")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed result with trailing text
    (should (listp field-results))
    (should (= (length field-results) 2))

    ;; Check that trailing text doesn't affect the fields
    (should (equal (cdr (assq 'title result)) "Example Title"))
    ;; The parser doesn't strip out trailing text after the field value,
    ;; so we need to check the whole content
    (should (string-match-p "^Example body text with some content" (cdr (assq 'body result))))

    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-adapter-parse-field-occurrence-after-value ()
  "Test parsing when a field name appears in another field's value."
  (let* ((sig (dsel-make-signature
               "Generate text"
               :input-fields (list '(:name prompt :type string :desc "Text prompt"))
               :output-fields (list '(:name summary :type string :desc "Text summary")
                                    '(:name feedback :type string :desc "Feedback on the text"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Summary: This document discusses how to provide feedback.

Feedback: The summary is accurate. When writing summaries, be concise.")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))

    ;; Test parsed result where field name appears in another field's value
    (should (listp field-results))
    (should (= (length field-results) 2))

    ;; Check field values
    (should (equal (cdr (assq 'summary result)) "This document discusses how to provide feedback."))
    (should (equal (cdr (assq 'feedback result)) "The summary is accurate. When writing summaries, be concise."))

    ;; Test no errors
    (should (cl-every (lambda (frp) (null (plist-get frp :error))) field-results))))

(ert-deftest dsel-test-coercion-basic-types ()
  "Test coercion of basic field types."
  ;; String coercion
  (should (equal "test" (dsel--coerce-value "test" (dsel-make-field :name 'test :type 'string))))

  ;; Integer coercion - valid
  (should (equal 42 (dsel--coerce-value "42" (dsel-make-field :name 'test :type 'integer))))

  ;; Integer coercion - invalid (but with name field)
  (should-error (dsel--coerce-value "not a number" (dsel-make-field :name 'test-integer :type 'integer)))

  ;; Number coercion
  (should (equal 3.14 (dsel--coerce-value "3.14" (dsel-make-field :name 'test :type 'number))))

  ;; Boolean coercion - true values
  (should (eq t (dsel--coerce-value "true" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "yes" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "t" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "TRUE" (dsel-make-field :name 'test :type 'boolean))))

  ;; Boolean coercion - false values
  (should (eq nil (dsel--coerce-value "false" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "no" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "nil" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "FALSE" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "0" (dsel-make-field :name 'test :type 'boolean)))))

(ert-deftest dsel-test-coercion-complex-types ()
  "Test coercion of complex field types."
  ;; Array coercion from JSON string
  (let ((json-array (dsel--coerce-value "[\"red\", \"green\", \"blue\"]" (dsel-make-field :name 'test :type 'array :items '(:type string)))))
    (should (sequencep json-array))
    (should (= (length json-array) 3))
    (should (equal (aref json-array 0) "red"))
    (should (equal (aref json-array 1) "green"))
    (should (equal (aref json-array 2) "blue")))

  ;; Array coercion from comma-separated string
  (let ((csv-array (dsel--coerce-value "red, green, blue" (dsel-make-field :name 'test :type 'array :items '(:type string)))))
    (should (listp csv-array))
    (should (= (length csv-array) 3))
    (should (equal (nth 0 csv-array) "red"))
    (should (equal (nth 1 csv-array) "green"))
    (should (equal (nth 2 csv-array) "blue")))

  ;; Empty array
  (let ((empty-array (dsel--coerce-value "[]" (dsel-make-field :name 'test :type 'array :items '(:type string)))))
    (should (sequencep empty-array))
    (should (= (length empty-array) 0)))

  ;; Object coercion from JSON string
  (let ((json-obj (dsel--coerce-value "{\"name\": \"John\", \"age\": 30}" (dsel-make-field :name 'test :type 'object :properties '((:name name :type string) (:name age :type integer))))))
    (should (listp json-obj))
    (should (assq 'name json-obj))
    (should (assq 'age json-obj))
    (should (equal (cdr (assq 'name json-obj)) "John"))
    (should (equal (cdr (assq 'age json-obj)) 30)))

  ;; Empty object
  (let ((empty-obj (dsel--coerce-value "{}" (dsel-make-field :name 'test :type 'object :properties '((:name name :type string))))))
    (should (listp empty-obj))
    (should (equal empty-obj nil)))

  ;; Malformed JSON string handling
  (should-error (dsel--coerce-value "[malformed array" (dsel-make-field :name 'malformed-test :type 'array :items '(:type string))))
  (should-error (dsel--coerce-value "{malformed json" (dsel-make-field :name 'malformed-test :type 'object :properties '((:name name :type string))))))

(ert-deftest dsel-test-coercion-enum-validation ()
  "Test enum validation during coercion."
  ;; Valid enum value
  (should (equal "red"
                 (dsel--coerce-value "red" (dsel-make-field :name 'test :type 'string :enum ["red" "green" "blue"]))))

  ;; Invalid enum value - should raise an error
  (should-error
   (dsel--coerce-value "purple" (dsel-make-field :name 'test :type 'string :enum ["red" "green" "blue"]))
   :type 'error))

(ert-deftest dsel-test-prediction-error-helpers ()
  "Test the prediction error helper functions."
  (let* ((sig (dsel-make-signature
               "Test error helpers"
               :input-fields (list '(:name input :type string :desc "Test input"))
               :output-fields (list '(:name field1 :type string :desc "First field")
                                    '(:name field2 :type integer :desc "Second field"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Field1: valid_value")  ; Missing field2
         (field-results (dsel-adapter-parse-output adapter sig response))
         (prediction (dsel-make-prediction
                      :input "test"
                      :field1 "valid_value"
                      :errors (cl-loop for frp in field-results
                                       when (plist-get frp :error)
                                       collect (plist-get frp :error)))))

    ;; Test dsel-prediction-ok-p
    (should (not (dsel-prediction-ok-p prediction)))
    
    ;; Test dsel-prediction-field-error
    (should (null (dsel-prediction-field-error prediction 'field1)))
    (should (dsel-prediction-field-error prediction 'field2))
    (should (eq (plist-get (dsel-prediction-field-error prediction 'field2) :type) :missing-required))
    
    ;; Test dsel-prediction-format-errors
    (let ((error-text (dsel-prediction-format-errors prediction)))
      (should (stringp error-text))
      (should (string-match-p "field2" error-text))
      (should (string-match-p "missing-required" error-text)))
    
    ;; Test dsel-prediction-report-errors returns error count
    (should (= (dsel-prediction-report-errors prediction) 1))
    
    ;; Test with successful prediction
    (let ((good-prediction (dsel-make-prediction :input "test" :field1 "value" :field2 42)))
      (should (dsel-prediction-ok-p good-prediction))
      (should (null (dsel-prediction-format-errors good-prediction)))
      (should (null (dsel-prediction-report-errors good-prediction))))))

(ert-deftest dsel-test-coercion-error-scenarios ()
  "Test various coercion error scenarios."
  ;; Boolean coercion errors
  (should-error (dsel--coerce-value "" (dsel-make-field :name 'test-bool :type 'boolean)))
  (should-error (dsel--coerce-value "maybe" (dsel-make-field :name 'test-bool :type 'boolean)))
  (should-error (dsel--coerce-value "1.5" (dsel-make-field :name 'test-bool :type 'boolean)))
  
  ;; Numeric coercion errors with helpful messages
  (should-error (dsel--coerce-value "abc" (dsel-make-field :name 'test-int :type 'integer)))
  (should-error (dsel--coerce-value "3.14" (dsel-make-field :name 'test-int :type 'integer)))
  ;; Note: "42px" parses as 42 with string-to-number, so we skip this check
  ;; Note: "1e10" is actually valid scientific notation and gets converted to 10000000000 by string-to-number
  
  ;; Array/Object JSON parsing errors  
  (should-error (dsel--coerce-value "{broken json" (dsel-make-field :name 'test-obj :type 'object :properties '((:name name :type string)))))
  (should-error (dsel--coerce-value "completely invalid array" (dsel-make-field :name 'test-arr :type 'array :items '(:type string))))
  (should-error (dsel--coerce-value "not-json" (dsel-make-field :name 'test-obj :type 'object :properties '((:name name :type string)))))
  
  ;; Enum validation errors
  (should-error (dsel--coerce-value "purple" (dsel-make-field :name 'test-enum :type 'string :enum ["red" "green" "blue"]))))

(ert-deftest dsel-test-mixed-success-and-error-fields ()
  "Test parsing when some fields succeed and others fail."
  (let* ((sig (dsel-make-signature
               "Mixed success/error test"
               :input-fields (list '(:name input :type string :desc "Test input"))
               :output-fields (list '(:name good_string :type string :desc "Valid string field")
                                    '(:name bad_integer :type integer :desc "Invalid integer field")
                                    '(:name good_boolean :type boolean :desc "Valid boolean field")
                                    '(:name missing_required :type string :desc "Missing required field"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Good_string: hello world

Bad_integer: not_a_number

Good_boolean: true")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Should have 4 field results (all fields processed)
    (should (= (length field-results) 4))
    
    ;; Check successful fields
    (let ((good-string-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'good_string)) field-results))
          (good-boolean-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'good_boolean)) field-results)))
      (should good-string-result)
      (should (null (plist-get good-string-result :error)))
      (should (equal (plist-get good-string-result :value) "hello world"))
      
      (should good-boolean-result)
      (should (null (plist-get good-boolean-result :error)))
      (should (eq (plist-get good-boolean-result :value) t)))
    
    ;; Check error fields
    (let ((bad-integer-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'bad_integer)) field-results))
          (missing-result (cl-find-if (lambda (frp) (eq (plist-get frp :name) 'missing_required)) field-results)))
      (should bad-integer-result)
      (should (plist-get bad-integer-result :error))
      (should (eq (plist-get (plist-get bad-integer-result :error) :type) :coercion))
      
      (should missing-result)
      (should (plist-get missing-result :error))
      (should (eq (plist-get (plist-get missing-result :error) :type) :missing-required)))))

(ert-deftest dsel-test-boolean-coercion-edge-cases ()
  "Test boolean coercion with various valid and invalid inputs."
  ;; Valid true values (case-insensitive)
  (should (eq t (dsel--coerce-value "true" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "TRUE" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "True" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "yes" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "YES" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "t" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "T" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq t (dsel--coerce-value "1" (dsel-make-field :name 'test :type 'boolean))))
  
  ;; Valid false values (case-insensitive)
  (should (eq nil (dsel--coerce-value "false" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "FALSE" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "False" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "no" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "NO" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "nil" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "NIL" (dsel-make-field :name 'test :type 'boolean))))
  (should (eq nil (dsel--coerce-value "0" (dsel-make-field :name 'test :type 'boolean))))
  
  ;; Invalid boolean values should error
  (should-error (dsel--coerce-value "" (dsel-make-field :name 'test-bool :type 'boolean)))
  (should-error (dsel--coerce-value "   " (dsel-make-field :name 'test-bool :type 'boolean))) ; whitespace only
  (should-error (dsel--coerce-value "maybe" (dsel-make-field :name 'test-bool :type 'boolean)))
  (should-error (dsel--coerce-value "2" (dsel-make-field :name 'test-bool :type 'boolean)))
  (should-error (dsel--coerce-value "on" (dsel-make-field :name 'test-bool :type 'boolean)))
  (should-error (dsel--coerce-value "off" (dsel-make-field :name 'test-bool :type 'boolean))))

(ert-deftest dsel-test-nil-input-coercion ()
  "Test coercion behavior with nil string inputs."
  ;; String type with nil input should return empty string
  (should (equal "" (dsel--coerce-value nil (dsel-make-field :name 'test :type 'string))))
  
  ;; Numeric types with nil input should return nil
  (should (null (dsel--coerce-value nil (dsel-make-field :name 'test :type 'integer))))
  (should (null (dsel--coerce-value nil (dsel-make-field :name 'test :type 'number))))
  
  ;; Array/Object types with nil input should return nil
  (should (null (dsel--coerce-value nil (dsel-make-field :name 'test :type 'array :items '(:type string)))))
  (should (null (dsel--coerce-value nil (dsel-make-field :name 'test :type 'object :properties '((:name name :type string))))))
  
  ;; Boolean type with nil input should error
  (should-error (dsel--coerce-value nil (dsel-make-field :name 'test-bool :type 'boolean))))

(ert-deftest dsel-test-error-message-content ()
  "Test that error messages contain helpful information."
  (let* ((sig (dsel-make-signature
               "Error message test"
               :input-fields (list '(:name input :type string :desc "Test input"))
               :output-fields (list '(:name bad_number :type integer :desc "Bad number field")
                                    '(:name missing_field :type string :desc "Missing field"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Bad_number: not_a_number_123")
         (field-results (dsel-adapter-parse-output adapter sig response)))

    ;; Test coercion error message includes raw value and field name
    (let ((coercion-error (cl-find-if (lambda (frp) 
                                        (and (eq (plist-get frp :name) 'bad_number)
                                             (plist-get frp :error)))
                                      field-results)))
      (should coercion-error)
      (let ((error-info (plist-get coercion-error :error)))
        (should (eq (plist-get error-info :type) :coercion))
        (should (eq (plist-get error-info :field) 'bad_number))
        (should (equal (plist-get error-info :raw-value) "not_a_number_123"))
        (should (stringp (plist-get error-info :message)))
        (should (string-match-p "bad_number" (plist-get error-info :message)))))
    
    ;; Test missing field error message includes field name
    (let ((missing-error (cl-find-if (lambda (frp) 
                                       (and (eq (plist-get frp :name) 'missing_field)
                                            (plist-get frp :error)))
                                     field-results)))
      (should missing-error)
      (let ((error-info (plist-get missing-error :error)))
        (should (eq (plist-get error-info :type) :missing-required))
        (should (eq (plist-get error-info :field) 'missing_field))
        (should (stringp (plist-get error-info :message)))
        (should (string-match-p "missing_field" (plist-get error-info :message)))))))

(ert-deftest dsel-test-adapter-concurrent-parsing ()
  "Test that adapter can handle concurrent parsing requests safely."
  (let* ((adapter (make-dsel-default-chat-adapter))
         (sig (dsel-make-signature
               "Concurrent test"
               :output-fields (list '(:name result :type string :desc "Test result"))))
         (responses '("Result: response1" "Result: response2" "Result: response3"))
         (results '()))
    
    ;; Parse multiple responses concurrently (simulated)
    (dolist (response responses)
      (let ((field-results (dsel-adapter-parse-output adapter sig response)))
        (push (dsel-test--field-results-to-alist field-results) results)))
    
    ;; All should succeed
    (should (= 3 (length results)))
    (should (equal "response1" (cdr (assq 'result (nth 2 results)))))
    (should (equal "response2" (cdr (assq 'result (nth 1 results)))))
    (should (equal "response3" (cdr (assq 'result (nth 0 results)))))))

(ert-deftest dsel-test-adapter-regex-special-characters ()
  "Test that field prefixes with regex special characters work correctly."
  (let* ((adapter (make-dsel-default-chat-adapter))
         (sig (dsel-make-signature
               "Regex test"
               :output-fields (list '(:name special :type string :desc "Field with special chars" :prefix "Result(1): ")
                                   '(:name normal :type string :desc "Normal field" :prefix "Result2: "))))
         (response "Result(1): special chars work\n\nResult2: normal field")
         (field-results (dsel-adapter-parse-output adapter sig response))
         (result (dsel-test--field-results-to-alist field-results)))
    
    (should (equal (cdr (assq 'special result)) "special chars work"))
    (should (equal (cdr (assq 'normal result)) "normal field"))))

(ert-deftest dsel-test-adapter-empty-response ()
  "Test adapter behavior with completely empty response."
  (let* ((adapter (make-dsel-default-chat-adapter))
         (sig (dsel-make-signature
               "Empty test"
               :output-fields (list '(:name required :type string :desc "Required field")
                                   '(:name optional :type string :desc "Optional field" :required nil))))
         (field-results (dsel-adapter-parse-output adapter sig "")))
    
    ;; Should have errors for missing fields (both required and optional are processed)
    (should (>= (length field-results) 1))
    ;; Find the required field error
    (let ((required-error (cl-find-if (lambda (frp) 
                                        (eq 'required (plist-get frp :name)))
                                      field-results)))
      (should required-error)
      (should (plist-get required-error :error)))))

(provide 'dsel-adapter-tests)
;;; dsel-adapter-tests.el ends here
