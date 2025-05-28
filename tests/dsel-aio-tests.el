;;; dsel-aio-tests.el --- async unit test suite for dsel-aio -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the vendored dsel-aio library to ensure it works correctly
;; without conflicting with user installations.

;; To run these tests:
;; $ emacs -batch -Q -l dsel-aio.el -l dsel-aio-tests.el -f ert-run-tests-batch

;; The tests run successfully in batch mode, testing all major functionality
;; of the async/await implementation including promises, timeouts, semaphores,
;; and process integration.

;;; Code:

(require 'dsel-aio)
(require 'cl-lib)
(require 'ert)
(require 'help)
(require 'help-fns)
(require 'rx)
(require 'sort)

(defmacro dsel-aio-with-test (timeout &rest body)
  "Run BODY asynchronously but block synchronously until it completes.

If TIMEOUT seconds passes without completion, signal an
`dsel-aio-timeout' to cause the test to fail."
  (declare (indent 1))
  `(let* ((promises (list (dsel-aio-with-async ,@body)
                          (dsel-aio-timeout ,timeout)))
          (select (dsel-aio-make-select promises)))
     (dsel-aio-wait-for
      (dsel-aio-with-async
        (dsel-aio-await (dsel-aio-await (dsel-aio-select select)))))))

;; Tests:

(ert-deftest dsel-test-aio-sleep ()
  "Test basic sleep functionality with return values."
  (dsel-aio-with-test 3
                      (let ((start (float-time)))
                        (dotimes (i 3)
                          (should (eql i
                                       (dsel-aio-await (dsel-aio-sleep 0.5 i)))))
                        (should (> (- (float-time) start)
                                   1.4)))))

(ert-deftest dsel-test-aio-repeat ()
  "Test that async functions can be called multiple times."
  (dsel-aio-with-test 3
                      (let ((sub (dsel-aio-lambda (result) (dsel-aio-await (dsel-aio-sleep .1 result)))))
                        (should (eq :a (dsel-aio-await (funcall sub :a))))
                        (should (eq :b (dsel-aio-await (funcall sub :b)))))))

(ert-deftest dsel-test-aio-timeout ()
  "Test timeout functionality and promise selection."
  (dsel-aio-with-test 4
                      ;; Test timeout wins
                      (let ((sleep (dsel-aio-sleep 1.0 t))
                            (timeout (dsel-aio-timeout 0.5))
                            (select (dsel-aio-make-select)))
                        (dsel-aio-select-add select sleep)
                        (dsel-aio-select-add select timeout)
                        (let ((winner (dsel-aio-await (dsel-aio-select select))))
                          (should (equal '(:error dsel-aio-timeout . 0.5)
                                         (dsel-aio-await (dsel-aio-catch winner))))))
                      ;; Test sleep wins
                      (let ((sleep (dsel-aio-sleep 0.1 t))
                            (timeout (dsel-aio-timeout 0.5))
                            (select (dsel-aio-make-select)))
                        (dsel-aio-select-add select sleep)
                        (dsel-aio-select-add select timeout)
                        (let ((winner (dsel-aio-await (dsel-aio-select select))))
                          (should (equal '(:success . t)
                                         (dsel-aio-await (dsel-aio-catch winner))))))))

(defun dsel-aio-test--shuffle (values)
  "Return a shuffled copy of VALUES."
  (let ((v (vconcat values)))
    (cl-loop for i from (1- (length v)) downto 1
             for j = (cl-random (+ i 1))
             do (cl-rotatef (aref v i) (aref v j))
             finally return (append v nil))))

(ert-deftest dsel-test-aio-sleep-sort ()
  "Test that promises resolve in correct time order (sleep sort)."
  (dsel-aio-with-test 8
                      (let* ((values (cl-loop for i from 5 to 60
                                              collect (/ i 20.0) into values
                                              finally return (dsel-aio-test--shuffle values)))
                             (count (length values))
                             (select (dsel-aio-make-select))
                             (promises (dolist (value values)
                                         (dsel-aio-select-add select (dsel-aio-sleep value value))))
                             (last 0.0))
                        (dotimes (_ count :done)
                          (let ((promise (dsel-aio-await (dsel-aio-select select))))
                            (let ((result (dsel-aio-await promise)))
                              (should (> result last))
                              (setf last result)))))))

(ert-deftest dsel-test-aio-process-sentinel ()
  "Test process sentinel integration with callbacks."
  (dsel-aio-with-test 10
                      (let ((process (start-process-shell-command "test" nil "exit 0"))
                            (sentinel (dsel-aio-make-callback)))
                        (setf (process-sentinel process) (car sentinel))
                        (should (equal "finished\n"
                                       (nth 1 (dsel-aio-chain (cdr sentinel))))))))

(ert-deftest dsel-test-aio-process-filter ()
  "Test process filter integration with callbacks."
  (dsel-aio-with-test 10
                      (let* ((command
                              (if (eq system-type 'windows-nt)
                                  (mapconcat #'identity
                                             '("echo a b c"
                                               "waitfor /t 1 x 2>nul"
                                               "echo 1 2 3"
                                               "waitfor /t 1 x 2>nul")
                                             "&")
                                "echo a b c; sleep 1; echo 1 2 3; sleep 1"))
                             (process (start-process-shell-command "test" nil command))
                             (filter (dsel-aio-make-callback)))
                        (setf (process-filter process) (car filter))
                        (should (equal "a b c\n"
                                       (nth 1 (dsel-aio-chain (cdr filter)))))
                        (should (equal "1 2 3\n"
                                       (nth 1 (dsel-aio-chain (cdr filter))))))))

(ert-deftest dsel-test-aio-sem ()
  "Test semaphore functionality for synchronization."
  (dsel-aio-with-test 5
                      (let ((n 64)
                            (sem (dsel-aio-sem 0))
                            (promises ())
                            (output ()))
                        (dotimes (i n)
                          ;; Queue up threads on the semaphore
                          (push
                           (dsel-aio-with-async
                             (dsel-aio-await (dsel-aio-sem-wait sem))
                             (push i output))
                           promises))
                        ;; Allow threads to run
                        (dotimes (_ n)
                          (dsel-aio-sem-post sem))
                        ;; Wait for all threads to complete (join)
                        (dsel-aio-await (dsel-aio-all promises))
                        ;; Check that the threads ran in correct order
                        (should (equal (number-sequence 0 63)
                                       (nreverse output))))))

(dsel-aio-defun dsel-aio-test-fun (foo &optional bar)
  "Reticulate the splines."
  (declare (obsolete nil nil))
  (interactive "sFoo: ")
  (list foo bar))

(ert-deftest dsel-test-aio-defun ()
  "Test that declarations and 'interactive' forms in 'dsel-aio-defun' work."
  (should (commandp 'dsel-aio-test-fun))
  (should (equal (interactive-form 'dsel-aio-test-fun) '(interactive "sFoo: ")))
  (should (equal (help-split-fundoc (documentation 'dsel-aio-test-fun)
                                    'dsel-aio-test-fun 'doc)
                 "Reticulate the splines."))
  (should (equal (gethash (indirect-function 'dsel-aio-test-fun) advertised-signature-table)
                 '(foo &optional bar)))
  (should (get 'dsel-aio-test-fun 'byte-obsolete-info)))

;; Additional tests specific to our vendored version

(ert-deftest dsel-test-aio-basic-promise ()
  "Test basic promise creation and resolution."
  (let ((promise (dsel-aio-promise)))
    (should (dsel-aio-promise-p promise))
    (should (null (dsel-aio-result promise)))

    (dsel-aio-resolve promise (lambda () 42))
    (should (funcall (dsel-aio-result promise)))
    (should (equal 42 (funcall (dsel-aio-result promise))))))

(ert-deftest dsel-test-aio-cancel ()
  "Test promise cancellation."
  (let ((promise (dsel-aio-promise)))
    (should (dsel-aio-cancel promise "test reason"))
    (should-error (funcall (dsel-aio-result promise)) :type 'dsel-aio-cancel)))

(ert-deftest dsel-test-aio-listen ()
  "Test promise listener functionality."
  (let ((promise (dsel-aio-promise))
        (result nil))
    (dsel-aio-listen promise (lambda (value) (setf result (funcall value))))
    (dsel-aio-resolve promise (lambda () :success))
    ;; Give time for the callback to run
    (sit-for 0.1)
    (should (eq result :success))))

(ert-deftest dsel-test-aio-idle ()
  "Test idle timer promise."
  (ert-skip "Idle timers don't work reliably in batch mode testing environments")
  (dsel-aio-with-test 5
                      (let ((start (float-time)))
                        (should (eq :idle-result
                                    (dsel-aio-await (dsel-aio-idle 0.1 :idle-result))))
                        (should (> (- (float-time) start) 0.05)))))

(ert-deftest dsel-test-aio-error-handling ()
  "Test error handling in async functions."
  (dsel-aio-with-test 2
                      (let ((error-fn (dsel-aio-lambda ()
                                        (error "Test error"))))
                        (should (equal '(:error error "Test error")
                                       (dsel-aio-await (dsel-aio-catch (funcall error-fn))))))))

(provide 'dsel-aio-tests)

;;; dsel-aio-tests.el ends here
