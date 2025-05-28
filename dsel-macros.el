;;; dsel-macros.el --- Definition macros for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides convenient definition macros for DSel components,
;; allowing for a more declarative and concise way to define signatures,
;; predictors, chain-of-thought modules, and optimizers.
;;
;; The macros define variables using `defvar` rather than using `setq`,
;; providing several benefits:
;; - Improved readability through declarative syntax
;; - Automatic docstrings for variable documentation
;; - Consistent naming across DSel components
;; - Reduced boilerplate code
;;
;; Usage examples:
;;
;; 1. Define a signature (replace the `setq` + `dsel-make-signature` pattern):
;;
;;    (dsel-defsignature sentiment-signature
;;      "Classify the sentiment of the given text as positive, negative, or neutral."
;;      :input-fields '((text . (:type string :desc "The text to classify")))
;;      :output-fields '((sentiment . (:type string :desc "The sentiment: positive, negative, or neutral"))))
;;
;; 2. Define a predictor:
;;
;;    (dsel-defpredict sentiment-predictor sentiment-signature
;;      :config '(:temperature 0.2))
;;
;; 3. Define a chain-of-thought module:
;;
;;    (dsel-defchain-of-thought cot-sentiment-predictor sentiment-signature
;;      :rationale-field-name 'reasoning
;;      :rationale-field-desc "Explain why you classified the text this way")
;;
;; 4. Define an optimizer:
;;
;;    (dsel-defoptimizer sentiment-optimizer labeled-fewshot
;;      :metric (lambda (gold pred)
;;               (string= (dsel-example-field gold 'sentiment)
;;                        (dsel-example-field pred 'sentiment)))
;;      :k 3)

;;; Code:

(require 'cl-lib)
(require 'dsel-types)
(require 'dsel-predictors)
(require 'dsel-optimizers)

;;; Utility functions for macro indentation and documentation

;;; Core definition macros

(defmacro dsel--defparameter (var value &optional docstring)
  "Define a parameter VAR with VALUE and optional DOCSTRING.
Update the parameter if it is already defined."
  (declare (indent 1))
  `(progn
     (if (boundp ',var)
         (setq ,var ,value)
       (defvar ,var ,value ,docstring))
     ',var))

(defmacro dsel-defsignature (name instructions &rest plist)
  "Define a DSel signature named NAME with INSTRUCTIONS and PLIST properties.
The signature is stored in the variable NAME.
PLIST is a keyword argument list for `dsel-make-signature'
(e.g., :input-fields ..., :output-fields ..., :name can be overridden).
If :name is not in PLIST, NAME is used as the signature's internal name."
  (declare (indent 2)) ; For proper indentation support
  `(dsel--defparameter ,name
     (dsel-make-signature
      ,instructions
      ,@(if (plist-member plist :name)
            plist
          (append (list :name (list 'quote name)) plist)))
     ,(format "DSel signature for %s: %s" name instructions)))

(defmacro dsel-defpredict (name signature-var &rest plist)
  "Define a DSel predictor named NAME using SIGNATURE-VAR and PLIST properties.
The predictor is stored in the variable NAME.
PLIST is a keyword argument list for `dsel-make-predict'
(e.g., :config ..., :demos ..., :lm ..., :name can be overridden).
If :name is not in PLIST, NAME is used as the predictor's internal name."
  (declare (indent 2))
  `(dsel--defparameter ,name
     (dsel-make-predict
      ,signature-var ; Assumes signature-var holds an already defined dsel-signature
      ,@(if (plist-member plist :name)
            plist
          (append (list :name (list 'quote name)) plist)))
     ,(format "DSel predictor %s using signature %s." name signature-var)))

;; In dsel-macros.el
(defmacro dsel-defexamples (var-name input-keys-form &rest example-definitions-plists)
  "Define a list of `dsel-example`s and assign to VAR-NAME.
Each example in EXAMPLE-DEFINITIONS-PLISTS is a plist of field-value pairs.
All created examples will have input keys from the evaluated INPUT-KEYS-FORM set.
INPUT-KEYS-FORM should evaluate to a list of symbols, e.g., '(key1 key2) or just 'key1."
  (declare (indent 2))
  (let ((docstring (format "A list of dsel-examples for %s with input keys derived from %s."
                           var-name input-keys-form)))
    `(dsel--defparameter ,var-name
       (dsel-create-examples
        ;; List of plists for each example definition
        ',(mapcar (lambda (def-plist) def-plist) example-definitions-plists)
        ;; Keyword argument for input-keys
        :input-keys ,input-keys-form)
       ,docstring)))

(defmacro dsel-defchain-of-thought (name original-signature &rest plist)
  "Define a DSel chain-of-thought module named NAME using ORIGINAL-SIGNATURE and PLIST.
The chain-of-thought module is stored in the variable NAME.
PLIST is a keyword argument list for `dsel-make-chain-of-thought'
(e.g., :rationale-field-name ..., :config ..., :name can be overridden).
If :name is not in PLIST, NAME is used as the module's internal name."
  (declare (indent 2))
  `(dsel--defparameter ,name
     (dsel-make-chain-of-thought
      ,original-signature
      ,@(if (plist-member plist :name)
            plist
          (append (list :name (list 'quote name)) plist)))
     ,(format "DSel chain-of-thought module %s using signature %s." name original-signature)))

(defalias 'dsel-defcot #'dsel-defchain-of-thought)

(defmacro dsel-defoptimizer (name type &rest plist)
  "Define a DSel optimizer named NAME of TYPE with PLIST properties.
The optimizer is stored in the variable NAME.
TYPE is a symbol like 'labeled-fewshot or 'bootstrap-fewshot.
PLIST is a keyword argument list for the corresponding make function
(e.g., `dsel-make-labeled-fewshot' or `dsel-make-bootstrap-fewshot').
If :name is in PLIST, it will be ignored as optimizers don't have internal names."
  (declare (indent 2))
  (let ((maker-function (intern (format "dsel-make-%s" (symbol-name type)))))
    `(dsel--defparameter ,name
       (,(if (fboundp maker-function)
             maker-function
           (error "No maker function for optimizer type %s" type))
        ,@plist)
       ,(format "DSel optimizer of type %s." type))))

(cl-defmacro dsel-defmodule (name &optional docstring &rest args)
  "Define a new module type NAME with custom slots and submodules.

This macro creates:
1. A new cl-struct that inherits from dsel-module (or specified parent)
2. A constructor function that initializes the module with its submodules

Arguments:
- NAME: The name of the module type (a symbol)
- DOCSTRING: Optional docstring for the struct (defaults to auto-generated)
- Keyword args and slots: The remaining arguments can be:
  - :include PARENT: The parent struct to inherit from (defaults to dsel-module)
  - :submodules ((NAME1 INIT-FORM1) (NAME2 INIT-FORM2) ...): Alist of submodule
     name-initializer pairs.
  - Additional SLOT specs as in `cl-defstruct`: (SLOT [DEFAULT] [:keyword VALUE]...)

Usage example:
  (dsel-defmodule my-rag-module
    \"A RAG module that combines retrieval and generation.\"
    :include dsel-module
    :submodules
    ((retriever (make-my-retriever :corpus 'my-docs))
     (generator (dsel-make-predict answer-signature)))
    (corpus nil :type symbol)
    (max-results 10 :type number))"
  (declare (indent 2))
  ;; Handle case where docstring is actually not a string
  (when (or (null docstring) (keywordp docstring) (listp docstring))
    (setq args (cons docstring args))
    (setq docstring nil))

  ;; Process args to separate keywords and slots
  (let* ((include-form 'dsel-module)
         (submodules nil)
         (slots nil)
         (temp-args args)
         (constructor-name (intern (format "make-%s" (symbol-name name))))
         (internal-constructor (intern (format "--%s" constructor-name)))
         (struct-docstring (or docstring (format "A custom DSel module of type %s." name)))
         (struct-name name))

    ;; Process keyword arguments
    (while (and temp-args (keywordp (car temp-args)))
      (let ((key (car temp-args))
            (value (cadr temp-args)))
        (cond
         ((eq key :include) (setq include-form value))
         ((eq key :submodules) (setq submodules value)))
        (setq temp-args (cddr temp-args))))

    ;; Remaining args are slots
    (setq slots temp-args)

    ;; Extract slot names and default values for constructor args
    (let* ((slot-specs (mapcar (lambda (slot-spec)
                                 (if (listp slot-spec)
                                     (list (car slot-spec) (cadr slot-spec))
                                   (list slot-spec nil)))
                               slots))
           ;; Construct the keyword args for the user-facing constructor
           (constructor-args
            (append
             ;; Add :name keyword arg with default to module name
             (list '&key (list 'name (list 'quote name)))
             ;; Add each submodule as a keyword arg with its default init-form
             (mapcar (lambda (sub)
                       (list (car sub) (cadr sub)))
                     submodules)
             ;; Add each custom slot as a keyword arg with its default init-form
             slot-specs)))

      ;; Build the macro expansion
      `(progn
         ;; Define the struct
         (cl-defstruct (,struct-name (:include ,include-form)
                                     (:constructor ,internal-constructor))
           ,struct-docstring
           ,@slots)

         ;; Define the user-facing constructor
         (cl-defun ,constructor-name ,constructor-args
           ,(format "Create a new %s module instance.
Arguments:
- :name the name of the module (defaults to '%s)
%s%s"
                    name name
                    (if submodules
                        (concat "- Submodules:\n"
                                (mapconcat
                                 (lambda (sub)
                                   (format "  - :%s a submodule (defaults to %s)"
                                           (car sub) (cadr sub)))
                                 submodules "\n"))
                      "")
                    (if slots
                        (concat (if submodules "\n" "")
                                "- Custom slots:\n"
                                (mapconcat
                                 (lambda (slot-spec)
                                   (let ((slot-name (if (listp slot-spec)
                                                        (car slot-spec)
                                                      slot-spec))
                                         (default-val (if (and (listp slot-spec) (> (length slot-spec) 1))
                                                          (cadr slot-spec)
                                                        "nil")))
                                     (format "  - :%s (defaults to %s)" slot-name default-val)))
                                 slots "\n"))
                      ""))

           ;; Create the struct instance with its submodules
           (let ((instance (,internal-constructor
                            :name name
                            ,@(apply #'append
                                     (mapcar (lambda (slot-spec)
                                               (let ((slot-name (if (listp slot-spec)
                                                                    (car slot-spec)
                                                                  slot-spec)))
                                                 (list (intern (format ":%s" slot-name))
                                                       slot-name)))
                                             slots)))))

             ;; Set up submodules as an alist
             (setf (dsel-module-submodules instance)
                   (list ,@(mapcar (lambda (sub)
                                     `(cons ',(car sub) ,(car sub)))
                                   submodules)))

             ;; Return the fully initialized instance
             instance))

         ;; Store the name of the custom module in a variable for reference
         (defvar ,name nil
           ,(format "Custom DSel module type '%s." name))

         ;; Return the module type name as a symbol
         ',name))))

(provide 'dsel-macros)
;;; dsel-macros.el ends here
