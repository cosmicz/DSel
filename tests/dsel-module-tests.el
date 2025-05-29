;;; dsel-module-tests.el --- Tests for dsel module macros -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for dsel-defmodule and dsel-defaforward macros

;;; Code:

(require 'ert)
(require 'dsel-module)
(require 'dsel-aio)
(require 'dsel-types)

;;; Test dsel-defmodule generation

(ert-deftest dsel-test-defmodule-basic ()
  "Test basic dsel-defmodule functionality."
  ;; Define a simple module with custom slots
  (dsel-defmodule test-simple-module
    "A simple test module."
    :submodules
    ((child1 (make-dsel-module :name 'child1))
     (child2 (make-dsel-module :name 'child2)))
    (custom-slot "default-value" :type string)
    (numeric-slot 42 :type number))

  ;; Verify the struct was created
  (should (fboundp 'test-simple-module-p))
  (should (fboundp 'make-test-simple-module))
  
  ;; Create an instance
  (let ((instance (make-test-simple-module)))
    ;; Check it's a module
    (should (dsel-module-p instance))
    (should (test-simple-module-p instance))
    
    ;; Check default values
    (should (eq (dsel-module-name instance) 'test-simple-module))
    (should (string= (test-simple-module-custom-slot instance) "default-value"))
    (should (= (test-simple-module-numeric-slot instance) 42))
    
    ;; Check submodules were set up
    (should (= (length (dsel-module-submodules instance)) 2))
    (should (dsel-module-get-submodule instance 'child1))
    (should (dsel-module-get-submodule instance 'child2))))

(ert-deftest dsel-test-defmodule-with-custom-args ()
  "Test dsel-defmodule with custom constructor arguments."
  (dsel-defmodule test-custom-module
    :submodules
    ((retriever (make-dsel-module :name 'retriever))
     (generator (make-dsel-module :name 'generator)))
    (max-results 5 :type number))

  ;; Create instance with custom args
  (let ((instance (make-test-custom-module 
                   :name 'my-custom-module
                   :max-results 10
                   :retriever (make-dsel-module :name 'custom-retriever))))
    
    (should (eq (dsel-module-name instance) 'my-custom-module))
    (should (= (test-custom-module-max-results instance) 10))
    
    ;; Check custom retriever was used
    (let ((retriever (dsel-module-get-submodule instance 'retriever)))
      (should retriever)
      (should (eq (dsel-module-name retriever) 'custom-retriever)))))

;;; Test dsel-defaforward with composite module

(ert-deftest dsel-test-defaforward-composite-module ()
  "Test dsel-defaforward with a composite module using multiple async steps."
  
  ;; Define mock submodules that return predictable promises
  (cl-defstruct (mock-retriever (:include dsel-module)))
  (cl-defstruct (mock-generator (:include dsel-module)))
  
  ;; Define aforward methods for mock modules
  (dsel-defaforward mock-retriever (&key query)
    "Mock retriever that returns documents."
    (dsel-make-prediction 
     :query query
     :documents (list "doc1" "doc2" "doc3")
     :lm-provider 'mock))
  
  (dsel-defaforward mock-generator (&key query documents)
    "Mock generator that creates responses."
    (dsel-make-prediction
     :query query
     :documents documents
     :response (format "Generated response for: %s using %d docs" 
                      query (length documents))
     :lm-provider 'mock))
  
  ;; Define composite RAG module
  (dsel-defmodule rag-module
    "A RAG module that combines retrieval and generation."
    :submodules
    ((retriever (make-mock-retriever :name 'retriever))
     (generator (make-mock-generator :name 'generator)))
    (max-docs 3 :type number))
  
  ;; Define aforward method for RAG module
  (dsel-defaforward rag-module (&key query)
    "Process query through retrieval and generation."
    (let* ((retrieval-result (dsel-aio-await (dsel-aforward 
                                              (cdr (assq 'retriever submodules))
                                              :query query)))
           (documents (dsel-get-field retrieval-result 'documents)))
      
      ;; Check if retrieval was successful
      (if (not (dsel-prediction-ok-p retrieval-result))
          ;; Return early with retrieval errors
          (dsel-make-prediction 
           :query query
           :errors (dsel-prediction-errors retrieval-result))
        
        ;; Continue with generation
        (let ((generation-result (dsel-aio-await (dsel-aforward 
                                                  (cdr (assq 'generator submodules))
                                                  :query query
                                                  :documents documents))))
          ;; Combine results
          (dsel-make-prediction
           :query query
           :documents documents
           :response (dsel-get-field generation-result 'response)
           :retrieval-metadata (list :source "mock-retriever")
           :generation-metadata (list :source "mock-generator")
           :errors (append (dsel-prediction-errors retrieval-result)
                          (dsel-prediction-errors generation-result)))))))
  
  ;; Test the composite module
  (let ((rag-instance (make-rag-module)))
    (let ((result (dsel-aio-wait-for (dsel-aforward rag-instance :query "test query"))))
      
      ;; Verify the prediction was created correctly
      (should (dsel-prediction-p result))
      (should (string= (dsel-get-field result 'query) "test query"))
      (should (equal (dsel-get-field result 'documents) (list "doc1" "doc2" "doc3")))
      (should (string-match-p "Generated response for: test query using 3 docs"
                             (dsel-get-field result 'response)))
      
      ;; Verify metadata was preserved
      (should (equal (dsel-get-field result 'retrieval-metadata) 
                    (list :source "mock-retriever")))
      (should (equal (dsel-get-field result 'generation-metadata) 
                    (list :source "mock-generator")))
      
      ;; Should have no errors
      (should (null (dsel-prediction-errors result))))))

;;; Test error propagation

(ert-deftest dsel-test-defaforward-error-propagation ()
  "Test error propagation in dsel-defaforward composite modules."
  
  ;; Define mock modules with error scenarios
  (cl-defstruct (failing-retriever (:include dsel-module)))
  (cl-defstruct (normal-generator (:include dsel-module)))
  
  ;; Retriever that returns errors
  (dsel-defaforward failing-retriever (&key query)
    "Mock retriever that fails."
    (dsel-make-prediction
     :query query
     :documents nil
     :errors (list (list :type :retrieval-failure
                        :message "Database connection failed"))))
  
  ;; Normal generator
  (dsel-defaforward normal-generator (&key query documents)
    "Mock generator that works normally."
    (dsel-make-prediction
     :query query
     :documents documents
     :response "Generated response"))
  
  ;; Define error-handling RAG module
  (dsel-defmodule error-rag-module
    "A RAG module that handles errors."
    :submodules
    ((retriever (make-failing-retriever :name 'failing-retriever))
     (generator (make-normal-generator :name 'normal-generator))))
  
  ;; Define aforward method that stops on retrieval errors
  (dsel-defaforward error-rag-module (&key query)
    "Process query with early error termination."
    (let ((retrieval-result (dsel-aio-await (dsel-aforward 
                                             (cdr (assq 'retriever submodules))
                                             :query query))))
      
      ;; Check for errors and stop early
      (if (not (dsel-prediction-ok-p retrieval-result))
          ;; Return immediately with retrieval errors
          (dsel-make-prediction
           :query query
           :stage "retrieval"
           :errors (dsel-prediction-errors retrieval-result))
        
        ;; This should not be reached in our test
        (error "Should not reach generation stage with failing retriever"))))
  
  ;; Test error propagation
  (let ((error-rag-instance (make-error-rag-module)))
    (let ((result (dsel-aio-wait-for (dsel-aforward error-rag-instance :query "test query"))))
      
      ;; Verify error was propagated
      (should (dsel-prediction-p result))
      (should (string= (dsel-get-field result 'query) "test query"))
      (should (string= (dsel-get-field result 'stage) "retrieval"))
      
      ;; Should have the retrieval error
      (should (not (dsel-prediction-ok-p result)))
      (let ((errors (dsel-prediction-errors result)))
        (should (= (length errors) 1))
        (should (eq (plist-get (car errors) :type) :retrieval-failure))
        (should (string= (plist-get (car errors) :message) "Database connection failed"))))))

(provide 'dsel-module-tests)
;;; dsel-module-tests.el ends here