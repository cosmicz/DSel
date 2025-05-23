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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result
    (should (listp result))
    (should (= (length result) 2))
    (should (equal (assq 'sentiment result) '(sentiment . "positive")))
    (should (equal (assq 'confidence result) '(confidence . 0.95)))))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed multiline values
    (should (listp result))
    (should (= (length result) 2))
    
    ;; Check code field
    (let ((code-value (cdr (assq 'code result))))
      (should (stringp code-value))
      (should (string-match-p "function factorial" code-value))
      (should (string-match-p "return n \\* factorial" code-value)))
    
    ;; Check explanation field
    (let ((explanation-value (cdr (assq 'explanation result))))
      (should (stringp explanation-value))
      (should (string-match-p "recursive implementation" explanation-value))
      (should (string-match-p "mathematical calculations" explanation-value)))))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed out-of-order fields
    (should (listp result))
    (should (= (length result) 3))
    
    ;; Check that all fields are present with correct values
    (should (equal (cdr (assq 'length result)) 42))
    (should (equal (cdr (assq 'sentiment result)) "positive"))
    (should (equal (cdr (assq 'summary result)) "This is a concise summary of the text."))))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result with missing optional field
    (should (listp result))
    (should (= (length result) 2))
    
    ;; Check present fields
    (should (equal (cdr (assq 'title result)) "Sample Document Analysis"))
    (should (equal (cdr (assq 'word_count result)) 1234))
    
    ;; Check missing optional field
    (should (null (assq 'author result)))))

(ert-deftest dsel-test-adapter-parse-missing-required-fields ()
  "Test parsing when required fields are missing from response."
  (let* ((sig (dsel-make-signature
               "Analyze document"
               :input-fields (list '(:name document :type string :desc "Document to analyze"))
               :output-fields (list '(:name title :type string :desc "Document title")
                                    '(:name category :type string :desc "Document category"))))
         (adapter (make-dsel-default-chat-adapter))
         (response "Title: Important Document"))

    ;; Expect an error because 'category' is required but missing
    (should-error (dsel-adapter-parse-output adapter sig response)
                  :type 'error)))

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

Word_count: "))

    ;; Expect an error because 'word_count' is required but empty (converted to nil)
    (should-error (dsel-adapter-parse-output adapter sig response)
                  :type 'error))

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

Categories: "))

    ;; Expect an error because 'categories' is required but empty (will be nil after coercion)
    (should-error (dsel-adapter-parse-output adapter sig response)
                  :type 'error))

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

Metadata: "))

    ;; Expect an error because 'metadata' is required but empty (will be nil after coercion)
    (should-error (dsel-adapter-parse-output adapter sig response)
                  :type 'error)))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result with empty values
    (should (listp result))
    (should (= (length result) 2)) ;; Now only 2 fields should be included

    ;; Check non-empty value
    (should (equal (cdr (assq 'name result)) "John Smith"))

    ;; Check empty values - string field should be empty string
    (should (equal (cdr (assq 'comments result)) ""))

    ;; Check empty values - number field should not be present in the result
    (should (eq nil (assq 'age result)))))

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
         (result (dsel-adapter-parse-output adapter sig response)))


    ;; Test parsed result with whitespace variations
    (should (listp result))
    (should (= (length result) 2))

    ;; Check values with leading/trailing spaces
    (should (equal (cdr (assq 'first_field result)) "Value with leading spaces"))
    (should (equal (cdr (assq 'second_field result)) "Value without space after prefix"))))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result with similar prefixes
    (should (listp result))
    (should (= (length result) 3))

    ;; Check each field's value - important to ensure "Note:" doesn't consume "Note_details:"
    (should (equal (cdr (assq 'note result)) "This is the main note content."))
    (should (equal (cdr (assq 'note_details result)) "These are the additional explanatory details."))
    (should (equal (cdr (assq 'summary result)) "Overall it says something important."))))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result with trailing text
    (should (listp result))
    (should (= (length result) 2))

    ;; Check that trailing text doesn't affect the fields
    (should (equal (cdr (assq 'title result)) "Example Title"))
    ;; The parser doesn't strip out trailing text after the field value,
    ;; so we need to check the whole content
    (should (string-match-p "^Example body text with some content" (cdr (assq 'body result))))))

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
         (result (dsel-adapter-parse-output adapter sig response)))

    ;; Test parsed result where field name appears in another field's value
    (should (listp result))
    (should (= (length result) 2))

    ;; Check field values
    (should (equal (cdr (assq 'summary result)) "This document discusses how to provide feedback."))
    (should (equal (cdr (assq 'feedback result)) "The summary is accurate. When writing summaries, be concise."))))

(ert-deftest dsel-test-coercion-basic-types ()
  "Test coercion of basic field types."
  ;; String coercion
  (should (equal "test" (dsel--coerce-value "test" '(:type string))))

  ;; Integer coercion - valid
  (should (equal 42 (dsel--coerce-value "42" '(:type integer))))

  ;; Integer coercion - invalid (but with name field)
  (should-error (dsel--coerce-value "not a number" '(:type integer :name test-integer)))

  ;; Number coercion
  (should (equal 3.14 (dsel--coerce-value "3.14" '(:type number))))

  ;; Boolean coercion - true values
  (should (eq t (dsel--coerce-value "true" '(:type boolean))))
  (should (eq t (dsel--coerce-value "yes" '(:type boolean))))
  (should (eq t (dsel--coerce-value "t" '(:type boolean))))
  (should (eq t (dsel--coerce-value "TRUE" '(:type boolean))))

  ;; Boolean coercion - false values
  (should (eq nil (dsel--coerce-value "false" '(:type boolean))))
  (should (eq nil (dsel--coerce-value "no" '(:type boolean))))
  (should (eq nil (dsel--coerce-value "nil" '(:type boolean))))
  (should (eq nil (dsel--coerce-value "FALSE" '(:type boolean))))
  (should (eq nil (dsel--coerce-value "0" '(:type boolean)))))

(ert-deftest dsel-test-coercion-complex-types ()
  "Test coercion of complex field types."
  ;; Array coercion from JSON string
  (let ((json-array (dsel--coerce-value "[\"red\", \"green\", \"blue\"]" '(:type array))))
    (should (sequencep json-array))
    (should (= (length json-array) 3))
    (should (equal (aref json-array 0) "red"))
    (should (equal (aref json-array 1) "green"))
    (should (equal (aref json-array 2) "blue")))

  ;; Array coercion from comma-separated string
  (let ((csv-array (dsel--coerce-value "red, green, blue" '(:type array))))
    (should (listp csv-array))
    (should (= (length csv-array) 3))
    (should (equal (nth 0 csv-array) "red"))
    (should (equal (nth 1 csv-array) "green"))
    (should (equal (nth 2 csv-array) "blue")))

  ;; Empty array
  (let ((empty-array (dsel--coerce-value "[]" '(:type array))))
    (should (sequencep empty-array))
    (should (= (length empty-array) 0)))

  ;; Object coercion from JSON string
  (let ((json-obj (dsel--coerce-value "{\"name\": \"John\", \"age\": 30}" '(:type object))))
    (should (listp json-obj))
    (should (assq 'name json-obj))
    (should (assq 'age json-obj))
    (should (equal (cdr (assq 'name json-obj)) "John"))
    (should (equal (cdr (assq 'age json-obj)) 30)))

  ;; Empty object
  (let ((empty-obj (dsel--coerce-value "{}" '(:type object))))
    (should (listp empty-obj))
    (should (equal empty-obj nil)))

  ;; Malformed JSON string handling
  (should-error (dsel--coerce-value "[malformed array" '(:type array :name malformed-test)))
  (should-error (dsel--coerce-value "{malformed json" '(:type object :name malformed-test))))

(ert-deftest dsel-test-coercion-enum-validation ()
  "Test enum validation during coercion."
  ;; Valid enum value
  (should (equal "red"
                 (dsel--coerce-value "red" '(:type string :enum ["red" "green" "blue"]))))

  ;; Invalid enum value - should raise an error
  (should-error
   (dsel--coerce-value "purple" '(:type string :enum ["red" "green" "blue"]))
   :type 'error))

(provide 'dsel-adapter-tests)
;;; dsel-adapter-tests.el ends here
