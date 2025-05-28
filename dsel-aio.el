;;; dsel-aio.el --- async/await for Emacs Lisp (vendored for dsel) -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;; Author: Christopher Wellons <wellons@nullprogram.com>
;; URL: https://github.com/skeeto/emacs-aio
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; `dsel-aio` is a vendored version of the aio library to avoid conflicts
;; with user installations. This is to Emacs Lisp as [`asyncio`][asyncio]
;; is to Python. This package builds upon Emacs 25 generators to provide
;; functions that pause while they wait on asynchronous events.  They do
;; not block any thread while paused.

;; The main components of this package are `dsel-aio-defun' / `dsel-aio-lambda'
;; to define async function, and `dsel-aio-await' to pause these functions
;; while they wait on asynchronous events.  When an asynchronous
;; function is paused, the main thread is not blocked.  It is no more
;; or less powerful than callbacks, but is nicer to use.

;; This is implementation is based on Emacs 25 generators, and
;; asynchronous functions are actually iterators in disguise, operated
;; as stackless, asymmetric coroutines.

;;; Code:

(require 'cl-lib)
(require 'font-lock)
(require 'generator)
(require 'macroexp)
(require 'rx)
(require 'iter2 nil t)

;; Register new error types
(define-error 'dsel-aio-cancel "Promise was canceled")
(define-error 'dsel-aio-timeout "Timeout was reached")

(cl-defstruct (dsel-aio-promise (:constructor dsel-aio-promise))
  "A promise object."
  result
  callbacks)

(defsubst dsel-aio-result (promise)
  "Return the result of PROMISE, or nil if it is unresolved.

Promise results are wrapped in a function.  The result must be
called (e.g. `funcall') in order to retrieve the value."
  (dsel-aio-promise-result promise))

(defun dsel-aio-listen (promise callback)
  "Add CALLBACK to PROMISE.

If the promise has already been resolved, the callback will be
scheduled for the next event loop turn."
  (let ((result (dsel-aio-result promise)))
    (if result
        (run-at-time 0 nil callback result)
      (push callback (dsel-aio-promise-callbacks promise)))))

(defun dsel-aio-resolve (promise value-function)
  "Resolve this PROMISE with VALUE-FUNCTION.

A promise can only be resolved once, and any further calls to
`dsel-aio-resolve' are silently ignored.  The VALUE-FUNCTION must be a
function that takes no arguments and either returns the result
value or rethrows a signal."
  (cl-check-type value-function function)
  (unless (dsel-aio-result promise)
    (let ((callbacks (nreverse (dsel-aio-promise-callbacks promise))))
      (setf (dsel-aio-promise-result promise) value-function
            (dsel-aio-promise-callbacks promise) ())
      (dolist (callback callbacks)
        (run-at-time 0 nil callback value-function)))))

(defun dsel-aio--step (iter promise yield-result)
  "Advance ITER to the next promise.

PROMISE is the return promise of the iterator, which was returned
by the originating async function.  YIELD-RESULT is the value
function result directly from the previously yielded promise."
  (condition-case _
      (cl-loop for result = (iter-next iter yield-result)
               then (iter-next iter (lambda () result))
               until (dsel-aio-promise-p result)
               finally (dsel-aio-listen result
                                        (lambda (value)
                                          (dsel-aio--step iter promise value))))
    (iter-end-of-sequence)))

(defmacro dsel-aio-with-promise (promise &rest body)
  "Evaluate BODY and resolve PROMISE with the result.

If the body signals an error, this error will be stored in the
promise and rethrown in the promise's listeners."
  (declare (indent defun)
           (debug (form body)))
  (cl-assert (eq lexical-binding t))
  `(dsel-aio-resolve ,promise
                     (condition-case error
                         (let ((result ,(macroexp-progn body)))
                           (lambda () result))
                       (error (lambda ()
                                (signal (car error) (cdr error)))))))

(defmacro dsel-aio-await (expr)
  "If EXPR evaluates to a promise, pause until the promise is resolved.

Pausing an async function does not block Emacs' main thread.  If
EXPR doesn't evaluate to a promise, the value is returned
immediately and the function is not paused.  Since async functions
return promises, async functions can await directly on other
async functions using this macro.

This macro can only be used inside an async function, either
`dsel-aio-lambda' or `dsel-aio-defun'."
  `(funcall (iter-yield ,expr)))

(defmacro dsel-aio-lambda (arglist &rest body)
  "Like `lambda', but defines an async function.

The body of this function may use `dsel-aio-await' to wait on
promises.  When an async function is called, it immediately
returns a promise that will resolve to the function's return
value, or any uncaught error signal.

See Info node '(elisp)Lambda Components' for a description of
ARGLIST and BODY."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define lambda-list lambda-doc
                           [&optional ("interactive" interactive)]
                           &rest sexp)))
  (let ((args (make-symbol "args"))
        (promise (make-symbol "promise"))
        (split-body (macroexp-parse-body body)))
    (if (fboundp 'iter2-lambda)
        `(lambda (&rest ,args)
           ,@(car split-body)
           (let* ((,promise (dsel-aio-promise))
                  (iter (apply (iter2-lambda ,arglist
                                             (dsel-aio-with-promise ,promise
                                               ,@(cdr split-body)))
                               ,args)))
             (prog1 ,promise
               (dsel-aio--step iter ,promise nil))))
      `(lambda (&rest ,args)
         ,@(car split-body)
         (let* ((,promise (dsel-aio-promise))
                (iter (apply (iter-lambda ,arglist
                               (dsel-aio-with-promise ,promise
                                 ,@(cdr split-body)))
                             ,args)))
           (prog1 ,promise
             (dsel-aio--step iter ,promise nil)))))))

(defmacro dsel-aio-defun (name arglist &rest body)
  "Like `dsel-aio-lambda' but gives the function a NAME like `defun'.

See Info node '(elisp)Defining Functions' for a description of
ARGLIST and BODY."
  (declare (indent defun)
           (doc-string 3)
           (debug (&define name lambda-list &rest sexp)))
  (or name (error "Cannot define `%s' as a function" name))
  (let* ((split-body (macroexp-parse-body body))
         (declarations (car split-body))
         (body (cdr split-body))
         (docstring (and (stringp (car declarations)) (pop declarations)))
         (declares (and (eq (car-safe (car declarations)) #'declare)
                        (cdr (pop declarations)))))
    ;; Other declarations (e.g., 'interactive' forms) are left as-is.
    `(progn
       (defalias ',name (dsel-aio-lambda ,arglist ,docstring ,@declarations ,@body))
       (function-put ',name 'dsel-aio-defun-p t)
       ,@(mapcar
          (lambda (declare)
            (let* ((prop (car declare))
                   (args (cdr declare))
                   (fun (assq prop defun-declarations-alist)))
              (or fun (error "Unknown 'defun' declaration property %s" prop))
              (apply (cadr fun) name arglist args)))
          declares)
       (set-advertised-calling-convention (indirect-function ',name) ',arglist nil)
       ',name)))

(defun dsel-aio-wait-for (promise)
  "Synchronously wait for PROMISE, blocking the current thread."
  (while (null (dsel-aio-result promise))
    (accept-process-output))
  (funcall (dsel-aio-result promise)))

(defun dsel-aio-cancel (promise &optional reason)
  "Attempt to cancel PROMISE, returning non-nil if successful.

All awaiters will receive an `dsel-aio-cancel' signal.  The actual
underlying asynchronous operation will not actually be canceled.
Optional argument REASON is used as error data for the signal."
  (unless (dsel-aio-result promise)
    (dsel-aio-resolve promise (lambda () (signal 'dsel-aio-cancel reason)))
    t))

(defmacro dsel-aio-with-async (&rest body)
  "Evaluate BODY asynchronously as if it was inside `dsel-aio-lambda'.

Since BODY is evalued inside an asynchronous lambda, `dsel-aio-await'
is available here.  This macro evaluates to a promise for BODY's
eventual result.

Beware: Dynamic bindings that are lexically outside
'dsel-aio-with-async' blocks have no effect.  For example,

  (defvar dynamic-var nil)
  (defun my-func ()
    (let ((dynamic-var 123))
      (dsel-aio-with-async dynamic-var)))
  (let ((dynamic-var 456))
    (dsel-aio-wait-for (my-func)))
  ⇒ 456

Other global state such as the current buffer behaves likewise."
  (declare (indent 0)
           (debug (&rest sexp)))
  `(let ((promise (funcall (dsel-aio-lambda ()
                             (dsel-aio-await (dsel-aio-sleep 0))
                             ,@body))))
     (prog1 promise
       ;; The is the main feature: Force the final result to be
       ;; realized so that errors are reported.
       (dsel-aio-listen promise #'funcall))))

(defmacro dsel-aio-chain (expr)
  "`dsel-aio-await' on EXPR and replace place EXPR with the next promise.

EXPR must be setf-able.  Returns (cdr result).  This macro is
intended to be used with `dsel-aio-make-callback' in order to follow
a chain of promise-yielding promises."
  (let ((result (make-symbol "result")))
    `(let ((,result (dsel-aio-await ,expr)))
       (setf ,expr (car ,result))
       (cdr ,result))))

;; Useful promise-returning functions:

(defmacro dsel-aio-all (promises)
  "Return a promise that resolves when all PROMISES are resolved."
  `(let ((promises ,promises))
     (while-let ((promise (pop promises)))
       (dsel-aio-await promise))))

(defun dsel-aio-catch (promise)
  "Return a new promise that wraps PROMISE but will never signal.

The promise value is a cons where the car is either :success or
:error.  For :success, the cdr will be the result value.  For
:error, the cdr will be the error data."
  (let ((result (dsel-aio-promise)))
    (cl-flet ((callback (value)
                (dsel-aio-resolve result
                                  (lambda ()
                                    (condition-case error
                                        (cons :success (funcall value))
                                      (error (cons :error error)))))))
      (prog1 result
        (dsel-aio-listen promise #'callback)))))

(defun dsel-aio-sleep (seconds &optional result)
  "Create a promise that is resolved after SECONDS with RESULT.

The result is a value, not a value function, and it will be
automatically wrapped with a value function (see `dsel-aio-resolve')."
  (let ((promise (dsel-aio-promise)))
    (prog1 promise
      (run-at-time seconds nil
                   #'dsel-aio-resolve promise (lambda () result)))))

(defun dsel-aio-idle (seconds &optional result)
  "Create a promise that is resolved after idle SECONDS with RESULT.

The result is a value, not a value function, and it will be
automatically wrapped with a value function (see `dsel-aio-resolve')."
  (let ((promise (dsel-aio-promise)))
    (prog1 promise
      (run-with-idle-timer seconds nil
                           #'dsel-aio-resolve promise (lambda () result)))))

(defun dsel-aio-timeout (seconds)
  "Create a promise with a timeout error after SECONDS."
  (let ((timeout (dsel-aio-promise)))
    (prog1 timeout
      (run-at-time seconds nil #'dsel-aio-resolve timeout
                   (lambda () (signal 'dsel-aio-timeout seconds))))))

(defun dsel-aio-url-retrieve (url &optional silent inhibit-cookies)
  "Wraps `url-retrieve' in a promise.

This function will never directly signal an error.  Instead any
errors will be delivered via the returned promise.  The promise
result is a cons of (status . buffer).  This buffer is a clone of
the buffer created by `url-retrieve' and should be killed by the
caller.

Arguments URL, SILENT, and INHIBIT-COOKIES are passed on to
`url-retrieve', which see.  Also see Info node '(url)Retrieving
URLs' for details."
  (require 'url)
  (let ((promise (dsel-aio-promise)))
    (prog1 promise
      (condition-case error
          (url-retrieve url (lambda (status)
                              (let ((value (cons status (clone-buffer))))
                                (dsel-aio-resolve promise (lambda () value))))
                        silent inhibit-cookies)
        (error (dsel-aio-resolve promise
                                 (lambda ()
                                   (signal (car error) (cdr error)))))))))

(cl-defun dsel-aio-make-callback (&key tag once)
  "Return a new callback function and its first promise.

Returns a cons (callback . promise) where callback is function
suitable for repeated invocation.  This makes it useful for
process filters and sentinels.  The promise is the first promise
to be resolved by the callback.

The promise resolves to:
  (next-promise . callback-args)
Or when TAG is supplied:
  (next-promise TAG . callback-args)
Or if ONCE is non-nil:
  callback-args

The callback resolves next-promise on the next invocation.  This
creates a chain of promises representing the sequence of calls.
Note: To avoid keeping lots of garbage in memory, avoid holding
onto the first promise (i.e. capturing it in a closure).

The `dsel-aio-chain' macro makes it easier to use these promises."
  (let* ((promise (dsel-aio-promise))
         (callback
          (if once
              (lambda (&rest args)
                (let ((result (if tag
                                  (cons tag args)
                                args)))
                  (dsel-aio-resolve promise (lambda () result))))
            (lambda (&rest args)
              (let* ((next-promise (dsel-aio-promise))
                     (result (if tag
                                 (cons next-promise (cons tag args))
                               (cons next-promise args))))
                (dsel-aio-resolve promise (lambda () result))
                (setf promise next-promise))))))
    (cons callback promise)))

;; A simple little queue

(defsubst dsel-aio--queue-empty-p (queue)
  "Return non-nil if QUEUE is empty.
An empty queue is (nil . nil)."
  (null (caar queue)))

(defsubst dsel-aio--queue-get (queue)
  "Get the next item from QUEUE, or nil for empty."
  (let ((head (car queue)))
    (cond ((null head)
           nil)
          ((eq head (cdr queue))
           (prog1 (car head)
             (setf (car queue) nil
                   (cdr queue) nil)))
          ((prog1 (car head)
             (setf (car queue) (cdr head)))))))

(defsubst dsel-aio--queue-put (queue element)
  "Append ELEMENT to QUEUE, returning ELEMENT."
  (let ((new (list element)))
    (prog1 element
      (if (null (car queue))
          (setf (car queue) new
                (cdr queue) new)
        (setf (cdr (cdr queue)) new
              (cdr queue) new)))))

;; An efficient select()-like interface for promises
(cl-defstruct (dsel-aio-select (:constructor dsel-aio--make-select))
  "A select() object for waiting on multiple promises."
  ;; Membership table
  (members (make-hash-table :test 'eq))
  ;; "Seen" table (avoid adding multiple callback)
  (seen (make-hash-table :test 'eq :weakness 'key))
  ;; Queue of pending resolved promises
  (queue (cons nil nil))
  ;; Callback to resolve select's own promise
  callback)

(defun dsel-aio-make-select (&optional promises)
  "Create a new `dsel-aio-select' object for waiting on multiple PROMISES."
  (let ((select (dsel-aio--make-select)))
    (prog1 select
      (dolist (promise promises)
        (dsel-aio-select-add select promise)))))

(defun dsel-aio-select-add (select promise)
  "Add PROMISE to the set of promises in SELECT.

SELECT is created with `dsel-aio-make-select'.  It is valid to add a
promise that was previously removed."
  (let ((members (dsel-aio-select-members select))
        (seen (dsel-aio-select-seen select)))
    (prog1 promise
      (unless (gethash promise seen)
        (setf (gethash promise seen) t
              (gethash promise members) t)
        (dsel-aio-listen promise
                         (lambda (_)
                           (when (gethash promise members)
                             (dsel-aio--queue-put (dsel-aio-select-queue select) promise)
                             (remhash promise members)
                             (let ((callback (dsel-aio-select-callback select)))
                               (when callback
                                 (setf (dsel-aio-select-callback select) nil)
                                 (funcall callback))))))))))

(defun dsel-aio-select-remove (select promise)
  "Remove PROMISE form the set of promises in SELECT.

SELECT is created with `dsel-aio-make-select'."
  (remhash promise (dsel-aio-select-members select)))

(defun dsel-aio-select-promises (select)
  "Return a list of promises in SELECT.

SELECT is created with `dsel-aio-make-select'."
  (cl-loop for key being the hash-keys of (dsel-aio-select-members select)
           collect key))

(defun dsel-aio-select (select)
  "Return a promise that resolves when any promise in SELECT resolves.

SELECT is created with `dsel-aio-make-select'.  This function is
level-triggered: if a promise in SELECT is already resolved, it
returns immediately with that promise.  Promises returned by
`dsel-aio-select' are automatically removed from SELECT.  Use this
function to repeatedly wait on a set of promises.

Note: The promise returned by this function resolves to another
promise, not that promise's result.  You will need to `dsel-aio-await'
on it, or use `dsel-aio-result'."
  (let* ((result (dsel-aio-promise))
         (callback (lambda ()
                     (let ((promise (dsel-aio--queue-get (dsel-aio-select-queue select))))
                       (dsel-aio-resolve result (lambda () promise))))))
    (prog1 result
      (if (dsel-aio--queue-empty-p (dsel-aio-select-queue select))
          (setf (dsel-aio-select-callback select) callback)
        (funcall callback)))))

;; Semaphores
(cl-defstruct (dsel-aio-sem (:constructor dsel-aio--make-sem))
  "A semaphore object."
  ;; Semaphore value
  (value 0)
  ;; Queue of waiting async functions
  (queue (cons nil nil)))

(defun dsel-aio-sem (init)
  "Create a new semaphore with initial value INIT."
  (dsel-aio--make-sem :value init))

(defun dsel-aio-sem-post (sem)
  "Increment the value of SEM.

If asynchronous functions are awaiting on SEM, then one will be
woken up.  This function is not awaitable."
  (when (<= (cl-incf (dsel-aio-sem-value sem)) 0)
    (let ((waiting (dsel-aio--queue-get (dsel-aio-sem-queue sem))))
      (when waiting
        (dsel-aio-resolve waiting (lambda () nil))))))

(defun dsel-aio-sem-wait (sem)
  "Decrement the value of SEM.

If SEM is at zero, returns a promise that will resolve when
another asynchronous function uses `dsel-aio-sem-post'."
  (when (< (cl-decf (dsel-aio-sem-value sem)) 0)
    (dsel-aio--queue-put (dsel-aio-sem-queue sem) (dsel-aio-promise))))

;; `emacs-lisp-mode' font lock

(font-lock-add-keywords
 'emacs-lisp-mode
 `((,(rx "(dsel-aio-defun" (+ blank)
         (group (+ (or (syntax word) (syntax symbol)))))
    1 'font-lock-function-name-face)))

(add-hook 'help-fns-describe-function-functions #'dsel-aio-describe-function)

(defun dsel-aio-describe-function (function)
  "Insert whether FUNCTION is an asynchronous function.
This function is added to 'help-fns-describe-function-functions'."
  (when (function-get function 'dsel-aio-defun-p)
    (insert "  This function is asynchronous; it returns "
            "an 'dsel-aio-promise' object.\n")))

(add-to-list 'lisp-imenu-generic-expression
             (list nil (concat "^\\s-*(dsel-aio-defun\\s-+\\(" lisp-mode-symbol-regexp "\\)") 1))

(provide 'dsel-aio)

;;; dsel-aio.el ends here
