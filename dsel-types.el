;;; dsel-types.el --- Core data structures for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the core data structures for DSel, including
;; signatures, examples, and predictions.

;;; Code:

(require 'cl-lib)

;;; Field

(cl-defstruct dsel-field
  "Represents a single input or output field in a DSel signature."
  (name nil :type symbol :read-only t)         ; Symbol: Unique identifier for the field
  (type nil :type symbol :read-only t)         ; Symbol: Data type (string, integer, boolean, array, object, etc.)
  (desc "" :type string)                       ; String: Natural language description
  (prefix nil :type (or null string))          ; String: Prefix for formatting (e.g., "Question: ")
  (optional nil :type boolean)                 ; Boolean: Is this field optional?
  (enum nil :type (or null vector))            ; Vector: Allowed values (if an enum type)
  ;; For array types:
  (items nil :type (or null dsel-field))       ; dsel-field struct for item type
  ;; For object types:
  (properties nil :type list)                  ; List of dsel-field structs for object properties
  (required nil :type list))                   ; List of symbols: Required property names for object type

(defun dsel-make-field (&rest field-plist-args)
  "Create and validate a dsel-field struct from FIELD-PLIST-ARGS.
This is the user-facing constructor that provides validation, defaults, and recursive processing."
  (let* ((field-name (plist-get field-plist-args :name))
         (field-type (plist-get field-plist-args :type))
         (field-desc (or (plist-get field-plist-args :desc) ""))
         (prefix (plist-get field-plist-args :prefix))
         (enum-spec (plist-get field-plist-args :enum))
         (items-spec (plist-get field-plist-args :items))
         (properties-spec (plist-get field-plist-args :properties))
         (optional (plist-get field-plist-args :optional))
         (required-props (plist-get field-plist-args :required)))

    ;; Basic validation
    (unless field-name (error "Field definition missing :name: %s" field-plist-args))
    (unless (symbolp field-name) (error "Field :name must be a symbol: %s" field-name))
    (unless field-type (error "Field '%s' must have a :type" field-name))
    (unless (symbolp field-type) (error "Field '%s' :type must be a symbol: %s" field-name field-type))

    (let ((resolved-enum nil)
          (processed-items nil)
          (processed-properties nil))

      ;; Resolve enum
      (when enum-spec
        (setq resolved-enum
              (cond
               ((symbolp enum-spec)
                (if (boundp enum-spec)
                    (symbol-value enum-spec)
                  (error "Field '%s': :enum symbol '%s' is unbound." field-name enum-spec)))
               ((listp enum-spec)
                (apply #'vector enum-spec))
               ((vectorp enum-spec)
                enum-spec)
               (t
                (error "Field '%s': :enum must be a vector, list, or symbol bound to one, got: %S" field-name enum-spec))))
        (unless (vectorp resolved-enum)
          (error "Field '%s': :enum must resolve to a vector, got: %S" field-name resolved-enum)))

      ;; Process nested items for arrays
      (when (eq field-type 'array)
        (unless items-spec (error "Array field '%s' must have :items" field-name))
        (unless (plist-get items-spec :type) (error "Array field '%s' :items must specify :type" field-name))
        ;; Array items don't need their own name, use a placeholder
        (setq processed-items (apply #'dsel-make-field :name '_array_item (append items-spec nil))))

      ;; Process nested properties for objects
      (when (eq field-type 'object)
        (unless properties-spec (error "Object field '%s' must have :properties" field-name))
        (setq processed-properties (mapcar (lambda (prop-plist)
                                             ;; Object properties should already have :name
                                             (apply #'dsel-make-field prop-plist))
                                           properties-spec)))

      ;; Default prefix
      (unless prefix
        (setq prefix (concat (capitalize (symbol-name field-name)) ":")))

      (make-dsel-field
       :name field-name
       :type field-type
       :desc field-desc
       :prefix prefix
       :optional optional
       :enum resolved-enum
       :items processed-items
       :properties processed-properties
       :required required-props))))

;;; Signature

(cl-defstruct dsel-signature
  "A structure representing an LLM task signature."
  name                                  ; Symbol, optional: A descriptive name for the signature
  instructions                          ; String: The high-level task instructions for the LLM
  input-fields                          ; List of dsel-field structs
  output-fields)                        ; List of dsel-field structs

(defun dsel-make-signature (instructions &rest plist)
  "Create a new signature with INSTRUCTIONS and properties from PLIST.
PLIST may include:
- :name A symbol for the signature name
- :input-fields List of field plists or alist of (name . plist) pairs
- :output-fields List of field plists or alist of (name . plist) pairs

Each field definition (plist or plist part of pair) must include:
- :name Symbol identifying the field (required if not key in alist)
- :type Expected type (e.g., 'string, 'integer, 'boolean, 'array, 'object) (required)

And may optionally include:
- :desc Natural language description of the field (default to empty string)
- :prefix String to prepend when formatting (defaults to capitalized :name)
- :format-fn Function to convert value to string (optional)
- :optional Boolean indicating if the field is optional (default is nil)
- :enum Vector of allowed values (optional)
- :items Property list describing array item type (required for 'array type, must include :type)
- :properties List of property field plists/alists (required for 'object type)
- :required List of required property name symbols (for 'object type)"
  (let* ((name (plist-get plist :name))
         (input-fields-arg (plist-get plist :input-fields))
         (output-fields-arg (plist-get plist :output-fields)))

    (unless (stringp instructions) (error "Instructions must be a string"))

    (cl-labels ((process-field-list (fields-list-or-alist)
                  (mapcar (lambda (field-def)
                            ;; If it's already a dsel-field struct, return as-is
                            (if (dsel-field-p field-def)
                                field-def
                              ;; Otherwise treat as plist and convert
                              (apply #'dsel-make-field field-def)))
                          (dsel-convert-alist-to-field-plists fields-list-or-alist))))

      (make-dsel-signature :name name
                           :instructions instructions
                           :input-fields (process-field-list input-fields-arg)
                           :output-fields (process-field-list output-fields-arg)))))

;;; Field conversion and access helpers

(defun dsel-convert-alist-to-field-plists (fields-alist)
  "Convert FIELDS-ALIST of field definitions to a list of field plists.
Handles the transition from old format ((field-name . field-plist) ...)
to new format ((:name field-name ...) ...)."
  (when fields-alist
    (if (and (consp (car fields-alist))
             (not (keywordp (car (car fields-alist)))))
        ;; Old alist format: ((field-name . field-plist) ...)
        (mapcar (lambda (pair)
                  (let ((name (car pair))
                        (plist (cdr pair)))
                    (plist-put plist :name name)))
                fields-alist)
      ;; Already in new format or empty
      fields-alist)))

(defun dsel-get-field-by-name (fields name)
  "Find a field with NAME in FIELDS list.
FIELDS is a list of dsel-field structs, NAME is a symbol."
  (cl-find-if (lambda (field) (eq (dsel-field-name field) name)) fields))

(defun dsel-signature-get-input-field (signature field-name)
  "Get the input field with FIELD-NAME from SIGNATURE."
  (dsel-get-field-by-name (dsel-signature-input-fields signature) field-name))

(defun dsel-signature-get-output-field (signature field-name)
  "Get the output field with FIELD-NAME from SIGNATURE."
  (dsel-get-field-by-name (dsel-signature-output-fields signature) field-name))

(defun dsel-field-names (fields)
  "Return a list of field names from FIELDS.
FIELDS is a list of dsel-field structs."
  (mapcar #'dsel-field-name fields))

(defun dsel-signature-input-field-names (signature)
  "Return a list of input field names from SIGNATURE."
  (dsel-field-names (dsel-signature-input-fields signature)))

(defun dsel-signature-output-field-names (signature)
  "Return a list of output field names from SIGNATURE."
  (dsel-field-names (dsel-signature-output-fields signature)))

;;; Example

(cl-defstruct dsel-example
  "A structure representing an example for few-shot learning."
  fields                                ; Alist of (field-name-symbol . value)
  input-keys)                           ; List of symbols denoting which keys are inputs

(defun dsel-make-example (&rest plist)
  "Create a new example from field-value pairs in PLIST.
Return a `dsel-example' with fields from PLIST and empty input-keys."
  (let ((fields nil)
        (plist-copy (copy-sequence plist)))
    (while plist-copy
      (let ((key (pop plist-copy))
            (value (pop plist-copy)))
        (when key  ; Always include the field even if value is nil
          ;; Convert :keyword to 'keyword
          (push (cons (dsel-keyword-to-symbol key) value) fields))))
    (make-dsel-example :fields (nreverse fields))))

(defun dsel-example-with-inputs (example &rest input-keys)
  "Return a new example based on EXAMPLE with INPUT-KEYS set."
  (let ((new-example (copy-dsel-example example)))
    (setf (dsel-example-input-keys new-example) input-keys)
    new-example))

(defun dsel-example-inputs (example)
  "Return an alist of input fields and their values from EXAMPLE."
  (let ((input-keys (dsel-example-input-keys example))
        (fields (dsel-example-fields example))
        (result nil))
    (dolist (key input-keys result)
      (let ((pair (assq key fields)))
        (when pair
          (push pair result))))
    (nreverse result)))

(defun dsel-create-examples (example-plists-list &key input-keys)
  "Create a list of `dsel-example's from EXAMPLE-PLISTS-LIST.
Optionally set common INPUT-KEYS for all created examples.
INPUT-KEYS should be a list of symbols, e.g., '(key1 key2) or just '(key1)."
  (mapcar
   (lambda (plist)
     (let ((example (apply #'dsel-make-example plist)))
       (if input-keys
           (apply #'dsel-example-with-inputs example input-keys)
         example)))
   example-plists-list))

(defun dsel-example-labels (example)
  "Return an alist of non-input (label) fields and values from EXAMPLE."
  (let ((input-keys (dsel-example-input-keys example))
        (fields (dsel-example-fields example))
        (result nil))
    (dolist (pair fields result)
      (unless (memq (car pair) input-keys)
        (push pair result)))
    (nreverse result)))

(defun dsel-example-field (example field-name-symbol)
  "Get the value of FIELD-NAME-SYMBOL from EXAMPLE."
  (cdr (assq field-name-symbol (dsel-example-fields example))))

(defalias 'dsel-get-field #'dsel-example-field
  "Get the value of FIELD-NAME-SYMBOL from an EXAMPLE-like object (e.g., dsel-example, dsel-prediction).
This is an alias for `dsel-example-field'.")

(defun dsel-set-example-field (example field-name-symbol value)
  "Set the value of FIELD-NAME-SYMBOL to VALUE in EXAMPLE."
  (let ((fields (dsel-example-fields example))
        (existing (assq field-name-symbol (dsel-example-fields example))))
    (if existing
        (setcdr existing value)
      (setf (dsel-example-fields example)
            (cons (cons field-name-symbol value) fields)))))

;;; Prediction

(cl-defstruct (dsel-prediction (:include dsel-example))
  "A structure representing a prediction from an LLM."
  completions                           ; List of alists for multiple generations
  lm-provider                           ; The llm.el provider instance used
  raw-response                          ; The raw string from the LLM
  (errors nil :type list))              ; List of error plists for parsing failures

(defun dsel-make-prediction (&rest plist)
  "Create a new prediction from properties in PLIST.
PLIST includes field-value pairs and may include:
- :completions List of alists for multiple generations
- :lm-provider The provider instance used
- :raw-response The raw string from the LLM
- :errors List of error plists for parsing failures"
  (let* ((completions (plist-get plist :completions))
         (lm-provider (plist-get plist :lm-provider))
         (raw-response (plist-get plist :raw-response))
         (errors (plist-get plist :errors))
         (fields nil)
         (plist-copy (copy-sequence plist)))

    ;; Process regular fields from plist
    (while plist-copy
      (let ((key (pop plist-copy))
            (value (pop plist-copy)))
        ;; Skip special properties and include all other fields
        (when (and key 
                   (not (memq key '(:lm-provider :raw-response :completions :errors))))
          ;; Convert :keyword to 'keyword for field names
          (push (cons (dsel-keyword-to-symbol key) value) fields))))
    
    (make-dsel-prediction :fields (nreverse fields)
                          :completions completions
                          :lm-provider lm-provider
                          :raw-response raw-response
                          :errors errors)))

(defun dsel-keyword-to-symbol (keyword)
  "Convert KEYWORD (e.g., :text) to a symbol (e.g., 'text).
If KEYWORD is already a symbol, return it unchanged.
If KEYWORD is not a keyword or symbol, signal an error."
  (cond
   ((keywordp keyword)
    (intern (substring (symbol-name keyword) 1)))
   ((symbolp keyword) ; Allow passing symbols through, idempotent
    keyword)
   (t
    (error "Argument is not a keyword or symbol: %s" keyword))))

(defun dsel-symbol-to-keyword (symbol)
  "Convert SYMBOL (e.g., 'text) to a keyword (e.g., :text).
If SYMBOL is already a keyword, return it unchanged.
If SYMBOL is not a symbol or keyword, signal an error."
  (cond
   ((symbolp symbol)
    (intern (concat ":" (symbol-name symbol))))
   ((keywordp symbol) ; Allow passing keywords through, idempotent
    symbol)
   (t
    (error "Argument is not a symbol or keyword: %s" symbol))))

;;; Prediction Helper Functions

(defun dsel-prediction-ok-p (prediction)
  "Return t if PREDICTION has no errors, nil otherwise."
  (null (dsel-prediction-errors prediction)))

(defun dsel-prediction-field-error (prediction field-name)
  "Search for an error related to FIELD-NAME in PREDICTION.
Return the error plist or nil if no error is found for that field."
  (cl-find-if (lambda (error-plist)
                (eq (plist-get error-plist :field) field-name))
              (dsel-prediction-errors prediction)))

(defun dsel-prediction-report-errors (prediction &optional prefix)
  "Report all errors in PREDICTION using `message'.
PREFIX is an optional string to prepend to each error message.
If PREFIX is nil, defaults to \"- \"."
  (let ((errors (dsel-prediction-errors prediction))
        (error-prefix (or prefix "- ")))
    (when errors
      (dolist (err errors)
        (message "%sField: %S, Type: %S, Message: %s%s"
                 error-prefix
                 (plist-get err :field)
                 (plist-get err :type)
                 (plist-get err :message)
                 (if (plist-get err :raw-value)
                     (format " (Raw: '%s')" (plist-get err :raw-value))
                   "")))
      (length errors))))

(defun dsel-prediction-format-errors (prediction &optional separator)
  "Format all errors in PREDICTION as a string.
SEPARATOR is used between error messages (defaults to newline).
Returns nil if there are no errors."
  (let ((errors (dsel-prediction-errors prediction)))
    (when errors
      (mapconcat
       (lambda (err)
         (format "Field: %S, Type: %S, Message: %s%s"
                 (plist-get err :field)
                 (plist-get err :type)
                 (plist-get err :message)
                 (if (plist-get err :raw-value)
                     (format " (Raw: '%s')" (plist-get err :raw-value))
                   "")))
       errors
       (or separator "\n")))))

(provide 'dsel-types)
;;; dsel-types.el ends here
