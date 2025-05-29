;;; dsel-llm.el --- LLM async integration for DSel -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides async LLM integration for DSel using dsel-aio primitives.
;; It wraps llm.el's callback-based async functions into promise-based APIs
;; that can be used with dsel-aio-await.

;;; Code:

(require 'llm)
(require 'dsel-aio)
(require 'dsel-types)

(defun dsel-llm-chat-aio (provider prompt-struct &optional config)
  "Call llm-chat-async and return a dsel-aio-promise for the response.
PROVIDER is an LLM provider instance from llm.el.
PROMPT-STRUCT is a llm-chat-prompt structure.
CONFIG is an optional configuration parameter.

Returns a dsel-aio-promise that resolves to the raw string response
from the LLM, or rejects with an error condition if the call fails.

The promise will:
- Resolve with the response string on success
- Reject with the error condition on failure

This function uses dsel-aio primitives to wrap the callback-based
llm-chat-async function into a promise-based API."
  (let ((promise (dsel-aio-promise)))
    (condition-case err
        (progn
          ;; Call llm-chat-async with success and error callbacks
          ;; Note: llm-fake doesn't support config parameters, so we only pass
          ;; config to real providers that support it
          (if (and config (not (llm-fake-p provider)))
              ;; Real provider that might support config - pass it through
              (llm-chat-async
               provider
               prompt-struct
               ;; Success callback - resolve the promise with the response
               (lambda (response)
                 (dsel-aio-resolve promise (lambda () response)))
               ;; Error callback - reject the promise with the error
               (lambda (error-type error-message)
                 (dsel-aio-resolve promise
                                   (lambda ()
                                     (signal error-type (list error-message)))))
               ;; Pass along config
               config)
            ;; No config or fake provider - use standard call
            (llm-chat-async
             provider
             prompt-struct
             ;; Success callback - resolve the promise with the response
             (lambda (response)
               (dsel-aio-resolve promise (lambda () response)))
             ;; Error callback - reject the promise with the error
             (lambda (error-type error-message)
               (dsel-aio-resolve promise
                                 (lambda ()
                                   (signal error-type (list error-message))))))))
      ;; Catch any immediate errors from llm-chat-async setup
      (error
       (dsel-aio-resolve promise
                         (lambda ()
                           (signal (car err) (cdr err))))))
    ;; Return the promise
    promise))

(defun dsel-llm-chat-sync (provider prompt-struct &optional config)
  "Synchronous wrapper around dsel-llm-chat-aio.
This is a convenience function that blocks until the async call completes.

Returns the response string directly, or signals an error on failure."
  (dsel-aio-wait-for (dsel-llm-chat-aio provider prompt-struct config)))

(defun dsel-llm-chat-aio-with-timeout (provider prompt-struct timeout &optional config)
  "Call dsel-llm-chat-aio with a timeout.
TIMEOUT is the maximum time to wait in seconds.

Returns a dsel-aio-promise that resolves to the response or rejects with
either the LLM error or a timeout error, whichever comes first."
  (dsel-aio-with-async
    (let* ((chat-promise (dsel-llm-chat-aio provider prompt-struct config))
           (timeout-promise (dsel-aio-timeout timeout))
           (select (dsel-aio-make-select (list chat-promise timeout-promise))))

      ;; Wait for whichever promise resolves first
      (let ((winner (dsel-aio-await (dsel-aio-select select))))
        (cond
         ;; If the chat promise won, return its result
         ((eq winner chat-promise)
          (dsel-aio-await chat-promise))
         ;; If the timeout promise won, signal timeout
         ((eq winner timeout-promise)
          (signal 'dsel-aio-timeout (list timeout)))
         ;; This shouldn't happen, but handle it gracefully
         (t
          (error "Unexpected winner in dsel-llm-chat-aio-with-timeout: %S" winner)))))))

(provide 'dsel-llm)

;;; dsel-llm.el ends here
