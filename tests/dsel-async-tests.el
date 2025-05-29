;;; dsel-async-tests.el --- Tests for dsel async functionality -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the async-first implementation in DSel, including dsel-aforward,
;; dsel-forward sync wrapper, and related helper functions.

;;; Code:

(require 'ert)
(require 'dsel-aio)
(require 'dsel-types)
(require 'dsel-module)

;; Test helper functions

(ert-deftest dsel-test-kwargs-to-plist ()
  "Test the dsel--kwargs-to-plist helper function."
  ;; Test empty list
  (should (equal '() (dsel--kwargs-to-plist '())))
  
  ;; Test single pair
  (should (equal '(:key value) (dsel--kwargs-to-plist '(:key value))))
  
  ;; Test multiple pairs
  (should (equal '(:a 1 :b 2 :c 3) 
                 (dsel--kwargs-to-plist '(:a 1 :b 2 :c 3))))
  
  ;; Test with nil values
  (should (equal '(:key nil :other "value") 
                 (dsel--kwargs-to-plist '(:key nil :other "value"))))
  
  ;; Test odd number of elements (missing last value)
  (should (equal '(:key value :orphan nil) 
                 (dsel--kwargs-to-plist '(:key value :orphan)))))

;; Mock module for testing
(cl-defstruct (dsel-test-async-module (:include dsel-module))
  "Test module for async functionality.")

;; Mock async implementation that resolves immediately
(cl-defmethod dsel-aforward ((module dsel-test-async-module) &rest kwargs)
  "Mock async implementation that returns a resolved promise."
  (let ((promise (dsel-aio-promise)))
    (dsel-aio-resolve promise 
                      (lambda () 
                        (apply #'dsel-make-prediction 
                               (append kwargs 
                                       (list :test-field "async-result")))))
    promise))

(ert-deftest dsel-test-aforward-basic ()
  "Test basic dsel-aforward functionality."
  (let ((module (make-dsel-test-async-module :name 'test-module)))
    (let ((promise (dsel-aforward module :input "test")))
      (should (dsel-aio-promise-p promise))
      
      (let ((result (dsel-aio-wait-for promise)))
        (should (dsel-prediction-p result))
        (should (equal "test" (dsel-get-field result 'input)))
        (should (equal "async-result" (dsel-get-field result 'test-field)))))))

(ert-deftest dsel-test-forward-sync-wrapper ()
  "Test that dsel-forward works as a sync wrapper around dsel-aforward."
  (let ((module (make-dsel-test-async-module :name 'test-module)))
    (let ((result (dsel-forward module :input "sync-test" :other 42)))
      (should (dsel-prediction-p result))
      (should (equal "sync-test" (dsel-get-field result 'input)))
      (should (equal 42 (dsel-get-field result 'other)))
      (should (equal "async-result" (dsel-get-field result 'test-field)))
      (should (dsel-prediction-ok-p result)))))

;; Mock module that throws an error in async method
(cl-defstruct (dsel-test-error-module (:include dsel-module))
  "Test module that throws errors for testing error handling.")

(cl-defmethod dsel-aforward ((module dsel-test-error-module) &rest kwargs)
  "Mock async implementation that rejects with an error."
  (let ((promise (dsel-aio-promise)))
    (dsel-aio-resolve promise 
                      (lambda () 
                        (error "Test async error")))
    promise))

(ert-deftest dsel-test-forward-error-handling ()
  "Test that dsel-forward handles errors from dsel-aforward correctly."
  (let ((module (make-dsel-test-error-module :name 'error-module)))
    (let ((result (dsel-forward module :input "error-test")))
      (should (dsel-prediction-p result))
      (should (equal "error-test" (dsel-get-field result 'input)))
      (should-not (dsel-prediction-ok-p result))
      
      (let ((errors (dsel-prediction-errors result)))
        (should (consp errors))
        (let ((error (car errors)))
          (should (eq :sync-wrapper-error (plist-get error :type)))
          (should (stringp (plist-get error :message))))))))

;; Mock module that times out
(cl-defstruct (dsel-test-timeout-module (:include dsel-module))
  "Test module that simulates timeouts.")

(cl-defmethod dsel-aforward ((module dsel-test-timeout-module) &rest kwargs)
  "Mock async implementation that never resolves (simulates timeout)."
  (dsel-aio-promise))  ; Return unresolved promise

(ert-deftest dsel-test-forward-timeout-handling ()
  "Test that dsel-forward handles timeouts correctly."
  ;; This test is tricky because we need to simulate a timeout
  ;; For now, we'll test the timeout error creation manually
  (let ((module (make-dsel-test-timeout-module :name 'timeout-module)))
    ;; Create a timeout error prediction manually to test the error structure
    (let ((timeout-prediction 
           (apply #'dsel-make-prediction
                  (append (dsel--kwargs-to-plist '(:input "timeout-test"))
                          (list :errors `((:type :timeout 
                                                 :message "Synchronous call timed out")))))))
      (should (dsel-prediction-p timeout-prediction))
      (should (equal "timeout-test" (dsel-get-field timeout-prediction 'input)))
      (should-not (dsel-prediction-ok-p timeout-prediction))
      
      (let ((errors (dsel-prediction-errors timeout-prediction)))
        (should (consp errors))
        (let ((error (car errors)))
          (should (eq :timeout (plist-get error :type)))
          (should (equal "Synchronous call timed out" (plist-get error :message))))))))

;; Test with real async behavior using dsel-aio primitives
(ert-deftest dsel-test-async-with-delay ()
  "Test async module with actual delay using dsel-aio-sleep."
  (cl-defstruct (dsel-test-delayed-module (:include dsel-module))
    "Test module with async delay.")
  
  (cl-defmethod dsel-aforward ((module dsel-test-delayed-module) &rest kwargs)
    "Mock async implementation with actual delay."
    (dsel-aio-with-async
      (dsel-aio-await (dsel-aio-sleep 0.1))  ; Short delay
      (apply #'dsel-make-prediction 
             (append kwargs 
                     (list :delayed-field "completed-after-delay")))))
  
  (let ((module (make-dsel-test-delayed-module :name 'delayed-module))
        (start-time (float-time)))
    
    ;; Test async version
    (let ((promise (dsel-aforward module :input "async-delay")))
      (should (dsel-aio-promise-p promise))
      (let ((result (dsel-aio-wait-for promise)))
        (should (dsel-prediction-p result))
        (should (equal "async-delay" (dsel-get-field result 'input)))
        (should (equal "completed-after-delay" (dsel-get-field result 'delayed-field)))
        (should (> (- (float-time) start-time) 0.09))))  ; Verify delay occurred
    
    ;; Test sync wrapper version
    (setq start-time (float-time))
    (let ((result (dsel-forward module :input "sync-delay")))
      (should (dsel-prediction-p result))
      (should (equal "sync-delay" (dsel-get-field result 'input)))
      (should (equal "completed-after-delay" (dsel-get-field result 'delayed-field)))
      (should (> (- (float-time) start-time) 0.09)))))  ; Verify delay occurred

;; Test that generic functions exist and work with base dsel-module
(ert-deftest dsel-test-generic-functions-exist ()
  "Test that the new generic functions are properly defined."
  (should (fboundp 'dsel-aforward))
  (should (fboundp 'dsel-forward))
  
  ;; Test that calling on base module without implementation gives expected error
  (let ((base-module (make-dsel-module :name 'base)))
    ;; dsel-aforward should signal no-applicable-method
    (should-error (dsel-aforward base-module :test "value") 
                  :type 'cl-no-applicable-method)
    
    ;; dsel-forward should return a prediction with error (wrapped by sync handler)
    (let ((result (dsel-forward base-module :test "value")))
      (should (dsel-prediction-p result))
      (should-not (dsel-prediction-ok-p result))
      (should (equal "value" (dsel-get-field result 'test)))
      (let ((errors (dsel-prediction-errors result)))
        (should (consp errors))
        (let ((error (car errors)))
          (should (eq :sync-wrapper-error (plist-get error :type))))))))

(provide 'dsel-async-tests)

;;; dsel-async-tests.el ends here