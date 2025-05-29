;;; dsel-predictors.el --- Predictor modules for dsel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides predictor modules for dsel, which handle the actual
;; LLM interactions and predictions.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'dsel-types)
(require 'dsel-module)
(require 'dsel-adapter)
(require 'dsel-settings)
(require 'dsel-aio)
(require 'dsel-llm)

(cl-defstruct (dsel-predict (:include dsel-module))
  "Structure for a basic LLM predictor."
  signature                             ; dsel-signature: signature for this LM call
  config                                ; plist: LM-specific parameters
  (demos nil :type list)                ; list of dsel-example: few-shot examples
  lm)                                   ; struct: specific llm.el provider

(cl-defun dsel-make-predict (signature &key name config demos lm)
  "Create a new predictor from SIGNATURE and keyword arguments.
Keyword arguments:
- NAME: symbol for the predictor name
- CONFIG: plist of LM-specific parameters
- DEMOS: list of few-shot examples
- LM: specific llm.el provider"
  (make-dsel-predict
   :name name
   :signature signature
   :config config
   :demos demos
   :lm lm
   :compiled-p nil
   :submodules nil))

(cl-defmethod dsel-collect-predictors ((predict dsel-predict))
  "For a dsel-predict, return a list containing just itself."
  (list predict))


(cl-defmethod dsel-aforward ((predict dsel-predict) &rest kwargs)
  "Asynchronously execute PREDICT with KWARGS and return a promise.
Returns a dsel-aio-promise that resolves to a dsel-prediction object."
  (dsel-aio-with-async
    (let* ((lm-to-use (or (dsel-predict-lm predict) dsel-settings--lm))
           (adapter-to-use (or dsel-settings--adapter
                               (make-dsel-default-chat-adapter)))
           (current-inputs-alist
            (let ((inputs nil))
              (while kwargs
                (let ((key (pop kwargs))
                      (value (pop kwargs)))
                  (when (and key value)
                    (push (cons (dsel-keyword-to-symbol key) value) inputs))))
              (nreverse inputs)))
           (merged-config (or (dsel-predict-config predict) nil))
           (llm-prompt (dsel-adapter-format-prompt
                        adapter-to-use
                        (dsel-predict-signature predict)
                        (dsel-predict-demos predict)
                        current-inputs-alist
                        merged-config))
           (raw-llm-response nil)
           (llm-call-error nil))

      ;; Perform async LLM call with error handling
      (condition-case err
          (setq raw-llm-response
                (dsel-aio-await (dsel-llm-chat-aio lm-to-use llm-prompt merged-config)))
        (error (setq llm-call-error err)))

      (let ((prediction
             (if llm-call-error
                 ;; LLM call failed - create prediction with error
                 (apply #'dsel-make-prediction
                        (append
                         ;; Include input fields
                         (cl-loop for (field-symbol . value) in current-inputs-alist
                                  collect (dsel-symbol-to-keyword field-symbol)
                                  collect value)
                         ;; Include LLM metadata and error
                         (list :lm-provider lm-to-use
                               :raw-response nil
                               :errors `((:type :llm-call
                                                :message ,(error-message-string llm-call-error)
                                                :error ,llm-call-error)))))
               ;; LLM call succeeded - parse response and create prediction
               (let* ((field-results (dsel-adapter-parse-output
                                      adapter-to-use
                                      (dsel-predict-signature predict)
                                      raw-llm-response))
                      (prediction-fields-alist nil)
                      (accumulated-errors nil))

                 ;; Process field results to separate successful fields from errors
                 (dolist (frp field-results)
                   (let ((field-name (plist-get frp :name))
                         (field-value (plist-get frp :value))
                         (field-error (plist-get frp :error)))
                     (if field-error
                         (push field-error accumulated-errors)
                       (push (cons field-name field-value) prediction-fields-alist))))

                 (apply #'dsel-make-prediction
                        (append
                         ;; Include input fields
                         (cl-loop for (field-symbol . value) in current-inputs-alist
                                  collect (dsel-symbol-to-keyword field-symbol)
                                  collect value)
                         ;; Include successfully parsed output fields
                         (cl-loop for (field-symbol . value) in prediction-fields-alist
                                  collect (dsel-symbol-to-keyword field-symbol)
                                  collect value)
                         ;; Include LLM metadata and parsing errors
                         (list :lm-provider lm-to-use
                               :raw-response raw-llm-response
                               :errors (nreverse accumulated-errors))))))))

        ;; Add to trace if enabled
        (when dsel-settings--trace
          (let ((trace-list (symbol-value dsel-settings--trace)))
            (set dsel-settings--trace
                 (cons (list predict current-inputs-alist prediction)
                       trace-list))))

        ;; Return the prediction (resolves the promise)
        prediction))))

(cl-defmethod dsel-module-reset-optimizable-state ((predict dsel-predict))
  "Reset 'compiled-p (from base) and demos for a dsel-predict instance."
  (setf (dsel-module-compiled-p predict) nil)
  (setf (dsel-predict-demos predict) nil))


(cl-defstruct (dsel-chain-of-thought (:include dsel-predict))
  "Structure for a chain-of-thought reasoning module that extends dsel-predict.")

(cl-defun dsel-make-chain-of-thought (original-signature &key name config demos lm
                                                         rationale-field-name
                                                         rationale-field-prefix
                                                         rationale-field-desc)
  "Create a new chain-of-thought module from ORIGINAL-SIGNATURE and keyword arguments.
Keyword arguments:
- NAME: symbol for the module name
- CONFIG: plist of LM-specific parameters
- DEMOS: list of dsel-example objects for few-shot prompting
- LM: specific llm.el provider
- RATIONALE-FIELD-NAME: symbol for the rationale field
- RATIONALE-FIELD-PREFIX: string prefix for the rationale field
- RATIONALE-FIELD-DESC: string description for the rationale field"
  (let* ((instructions (dsel-signature-instructions original-signature))
         (sig-name (dsel-signature-name original-signature))
         (input-fields (dsel-signature-input-fields original-signature))
         (output-fields (dsel-signature-output-fields original-signature))
         (rationale-field-name (or rationale-field-name 'rationale))
         (rationale-field-prefix (or rationale-field-prefix "Rationale:"))
         (rationale-field-desc (or rationale-field-desc "Your step-by-step reasoning process"))
         ;; Create rationale field as struct
         (rationale-field (dsel-make-field :name rationale-field-name
                                           :type 'string
                                           :desc rationale-field-desc
                                           :prefix rationale-field-prefix))
         ;; Create new signature by copying the original and adding rationale field
         (cot-signature (make-dsel-signature
                         :instructions instructions
                         :name sig-name
                         :input-fields input-fields
                         :output-fields (cons rationale-field output-fields))))
    (make-dsel-chain-of-thought
     :name name
     :signature cot-signature
     :lm lm
     :config config
     :demos demos
     :compiled-p nil
     :submodules nil)))

;; Chain-of-thought aliases
(defalias 'dsel-make-cot #'dsel-make-chain-of-thought)

;; For the struct, we need to create constructor aliases manually since cl-defstruct
;; creates functions with specific names that can't be easily aliased
(defalias 'dsel-cot-p #'dsel-chain-of-thought-p)
(defalias 'dsel-cot-signature #'dsel-chain-of-thought-signature)
(defalias 'dsel-cot-config #'dsel-chain-of-thought-config)
(defalias 'dsel-cot-demos #'dsel-chain-of-thought-demos)
(defalias 'dsel-cot-lm #'dsel-chain-of-thought-lm)

(provide 'dsel-predictors)
;;; dsel-predictors.el ends here
