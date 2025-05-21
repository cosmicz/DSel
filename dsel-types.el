;;; dsel-types.el --- Core data structures for DSel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the core data structures for DSel, including
;; signatures, examples, and predictions.

;;; Code:

(require 'cl-lib)

;;; Signature

(cl-defstruct dsel-signature
  "A structure representing an LLM task signature.
Following JSON Schema conventions, fields are defined as plists with :name, :type, etc."
  name                                  ; Symbol, optional: A descriptive name for the signature
  instructions                          ; String: The high-level task instructions for the LLM
  input-fields                          ; List of field plists, each with :name, :type, :desc, etc.
  output-fields)                        ; List of field plists, each with :name, :type, :desc, etc.

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

    (cl-labels ((process-one-field (field-plist)
                  (let* ((field-name (plist-get field-plist :name))
                         (field-type (plist-get field-plist :type))
                         ;; :desc is now optional, defaults to "" if not provided
                         (field-desc (or (plist-get field-plist :desc) ""))
                         (prefix (plist-get field-plist :prefix))
                         (processed-plist (copy-tree field-plist)))

                    (unless field-name (error "Field definition missing :name: %s" field-plist))
                    (unless (symbolp field-name) (error "Field :name must be a symbol: %s" field-name))
                    ;; (unless field-desc (error "Field '%s' must have a :desc" field-name)) ; Removed this check
                    (unless field-type (error "Field '%s' must have a :type" field-name))

                    ;; Ensure :desc is in the plist, even if it's the default ""
                    (setq processed-plist (plist-put processed-plist :desc field-desc))

                    (cond
                     ((eq field-type 'array)
                      (let* ((items-plist (plist-get processed-plist :items)))
                        (unless items-plist (error "Array field '%s' must have :items" field-name))
                        (unless (plist-get items-plist :type) (error "Array field '%s' :items must specify :type" field-name))
                        (when (eq (plist-get items-plist :type) 'object)
                          (let ((item-props (plist-get items-plist :properties)))
                            (unless item-props (error "Array field '%s' :items of type object must have :properties" field-name))
                            (setq processed-plist
                                  (plist-put processed-plist :items
                                             (plist-put items-plist :properties (process-field-list item-props))))))))
                     ((eq field-type 'object)
                      (let ((properties (plist-get processed-plist :properties)))
                        (unless properties (error "Object field '%s' must have :properties" field-name))
                        (setq processed-plist (plist-put processed-plist :properties (process-field-list properties))))))

                    (unless prefix
                      ;; Don't include a space after the colon for more flexible matching
                      (setq prefix (concat (capitalize (symbol-name field-name)) ":"))
                      (setq processed-plist (plist-put processed-plist :prefix prefix)))

                    processed-plist))

                (process-field-list (fields-list-or-alist)
                  (mapcar #'process-one-field (dsel-convert-alist-to-field-plists fields-list-or-alist))))

      (make-dsel-signature :name name
                           :instructions instructions
                           :input-fields (process-field-list input-fields-arg)
                           :output-fields (process-field-list output-fields-arg)))))

;;; Field conversion and access helpers

(defun dsel-convert-alist-to-field-plists (fields-alist)
  "Convert an alist of field definitions to a list of field plists.
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
FIELDS is a list of field plists, NAME is a symbol."
  (cl-find-if (lambda (field) (eq (plist-get field :name) name)) fields))

(defun dsel-signature-get-input-field (signature field-name)
  "Get the input field with FIELD-NAME from SIGNATURE."
  (dsel-get-field-by-name (dsel-signature-input-fields signature) field-name))

(defun dsel-signature-get-output-field (signature field-name)
  "Get the output field with FIELD-NAME from SIGNATURE."
  (dsel-get-field-by-name (dsel-signature-output-fields signature) field-name))

(defun dsel-field-names (fields)
  "Return a list of field names from FIELDS."
  (mapcar (lambda (field) (plist-get field :name)) fields))

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
  raw-response)                         ; The raw string from the LLM

(defun dsel-make-prediction (&rest plist)
  "Create a new prediction from properties in PLIST.
PLIST includes field-value pairs and may include:
- :completions List of alists for multiple generations
- :lm-provider The provider instance used
- :raw-response The raw string from the LLM"
  (let* ((completions (plist-get plist :completions))
         (lm-provider (plist-get plist :lm-provider))
         (raw-response (plist-get plist :raw-response))
         (fields nil)
         (plist-copy (copy-sequence plist)))
    
    ;; Process regular fields from plist
    (while plist-copy
      (let ((key (pop plist-copy))
            (value (pop plist-copy)))
        ;; Skip special properties and include all other fields
        (when (and key 
                   (not (memq key '(:lm-provider :raw-response :completions))))
          ;; Convert :keyword to 'keyword for field names
          (push (cons (dsel-keyword-to-symbol key) value) fields))))
    
    (make-dsel-prediction :fields (nreverse fields)
                          :completions completions
                          :lm-provider lm-provider
                          :raw-response raw-response)))

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

(provide 'dsel-types)
;;; dsel-types.el ends here
