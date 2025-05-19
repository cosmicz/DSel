;;; dsel-predictors.el --- Predictor modules for dsel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
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

(cl-defstruct (dsel-predict (:include dsel-module))
  "Structure for a basic LLM predictor."
  signature                             ; dsel-signature: signature for this LM call
  config                                ; plist: LM-specific parameters
  (demos nil :type list)                ; list of dsel-example: few-shot examples
  lm)                                   ; struct: specific llm.el provider

(cl-defmethod dsel-collect-predictors ((predict dsel-predict))
  "For a dsel-predict, return a list containing just itself."
  (list predict))

(cl-defmethod dsel-forward ((predict dsel-predict) &rest kwargs)
  "Execute PREDICT with KWARGS and return a prediction."
  (message "DSFWD: Entered for predict: %S, kwargs: %S" predict kwargs)
  (let* ((lm-to-use (or (dsel-predict-lm predict) dsel-settings--lm))
         (adapter-to-use (or dsel-settings--adapter
                             (make-dsel-default-chat-adapter)))
         ;; Convert kwargs to inputs-alist
         (current-inputs-alist
          (let ((inputs nil))
            (message "Initial predict object for forward: %S" predict)
            (while kwargs
              (let ((key (pop kwargs))
                    (value (pop kwargs)))
                (when (and key value)
                  (push (cons (dsel-keyword-to-symbol key) value) inputs))))
            (nreverse inputs)))


         (_ (message "DSFWD: current-inputs-alist: %S" current-inputs-alist))

         ;; Format the prompt
         (llm-prompt (dsel-adapter-format-prompt
                      adapter-to-use
                      (dsel-predict-signature predict)
                      (dsel-predict-demos predict)
                      current-inputs-alist))

         (_ (message "DSFWD: llm-prompt: %S" llm-prompt))

         ;; Merge config with defaults
         (merged-config (or (dsel-predict-config predict) nil))

         (_ (message "DSFWD: making LLM call"))

         ;; Make the LLM call
         (raw-llm-response (llm-chat lm-to-use llm-prompt merged-config))

         (_ (message "DSFWD: raw-llm-response: %S" raw-llm-response))

         ;; Parse the response
         (parsed-outputs-alist
          (dsel-adapter-parse-output
           adapter-to-use
           (dsel-predict-signature predict)
           raw-llm-response))

         (_ (message "DSFWD: parsed-outputs-alist: %S" parsed-outputs-alist))

         (prediction
          (apply #'dsel-make-prediction
                 (append
                  ;; Convert keys in current-inputs-alist and parsed-outputs-alist
                  ;; to keywords for dsel-make-prediction's &rest plist
                  (cl-loop for (field-symbol . value) in current-inputs-alist
                           collect (dsel-symbol-to-keyword field-symbol) ; 'a -> :a
                           collect value)
                  (cl-loop for (field-symbol . value) in parsed-outputs-alist
                           collect (dsel-symbol-to-keyword field-symbol) ; 'b -> :b
                           collect value)
                  ;; Prediction-specific keywords are already keywords
                  (list :lm-provider lm-to-use
                        :raw-response raw-llm-response)))))

    ;; Record in trace if enabled
    (when dsel-settings--trace
      (push (list predict current-inputs-alist prediction)
            dsel-settings--trace))

    ;; Return the prediction
    prediction))

(cl-defmethod dsel-module-reset-optimizable-state ((predict dsel-predict))
  "Reset 'compiled-p (from base) and demos for a dsel-predict instance."
  ;; Explicitly do what the base method would do (or call a helper if it were complex)
  (setf (dsel-module-compiled-p predict) nil)
  ;; Then do dsel-predict specific things
  (setf (dsel-predict-demos predict) nil))

(defun dsel-make-predict (signature &rest plist)
  "Create a new predictor from SIGNATURE and properties in PLIST.
  PLIST may include:
  - :config plist of LM-specific parameters
  - :demos list of few-shot examples
  - :lm specific llm.el provider
  - :name symbol for the predictor name"
  (make-dsel-predict
   :name (plist-get plist :name)
   :signature signature
   :config (plist-get plist :config)
   :demos (plist-get plist :demos)
   :lm (plist-get plist :lm)
   :compiled-p nil
   :predictors nil))

(cl-defstruct (dsel-chain-of-thought (:include dsel-module))
  "Structure for a chain-of-thought reasoning module."
  predictor                             ; dsel-predict: the underlying predictor
  cot-signature)                        ; dsel-signature: modified signature with rationale

(cl-defmethod dsel-collect-predictors ((cot dsel-chain-of-thought))
  "For a chain-of-thought module, return its predictor."
  (list (dsel-chain-of-thought-predictor cot)))

(cl-defmethod dsel-forward ((cot dsel-chain-of-thought) &rest kwargs)
  "Execute COT with KWARGS and return a prediction."
  (apply #'dsel-forward (dsel-chain-of-thought-predictor cot) kwargs))

(defun dsel-make-chain-of-thought (original-signature &rest plist)
  "Create a new chain-of-thought module from ORIGINAL-SIGNATURE and PLIST.
  PLIST may include:
  - :rationale-field-name symbol for the rationale field
  - :rationale-field-prefix string prefix for the rationale field
  - :rationale-field-desc string description for the rationale field
  - :lm specific llm.el provider
  - :config plist of LM-specific parameters
  - :name symbol for the module name"
  (let* ((instructions (dsel-signature-instructions original-signature))
         (sig-name (dsel-signature-name original-signature))
         (input-fields (dsel-signature-input-fields original-signature))
         (output-fields (dsel-signature-output-fields original-signature))
         (rationale-field-name (or (plist-get plist :rationale-field-name)
                                   'rationale))
         (rationale-field-prefix (or (plist-get plist :rationale-field-prefix)
                                     "Rationale: "))
         (rationale-field-desc (or (plist-get plist :rationale-field-desc)
                                   "Your step-by-step reasoning process"))
         (rationale-field-plist `(:type string
                                        :desc ,rationale-field-desc
                                        :prefix ,rationale-field-prefix))

         ;; Create COT signature by prepending rationale field
         (cot-signature (dsel-make-signature
                         instructions
                         :name sig-name
                         :input-fields input-fields
                         :output-fields (cons (cons rationale-field-name rationale-field-plist)
                                              output-fields))))

    ;; Create the predictor with the COT signature
    (let ((predictor (dsel-make-predict
                      cot-signature
                      :lm (plist-get plist :lm)
                      :config (plist-get plist :config))))

      ;; Create and return the COT module
      (make-dsel-chain-of-thought
       :name (plist-get plist :name)
       :predictor predictor
       :cot-signature cot-signature
       :compiled-p nil
       :predictors nil))))  ; Initialize with empty predictors list

(provide 'dsel-predictors)
;;; dsel-predictors.el ends here
