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
  "A structure representing an LLM task signature."
  name                                  ; Symbol, optional: A descriptive name for the signature
  instructions                          ; String: The high-level task instructions for the LLM
  input-fields                          ; Alist of (field-name-symbol . field-plist)
  output-fields)                        ; Alist of (field-name-symbol . field-plist)

(defun dsel-make-signature (instructions &rest plist)
  "Create a new signature with INSTRUCTIONS and properties from PLIST.
PLIST may include:
- :name A symbol for the signature name
- :input-fields Alist of (field-name-symbol . field-plist)
- :output-fields Alist of (field-name-symbol . field-plist)

Each field-plist may include:
- :type Expected Elisp type (e.g., 'string, 'integer)
- :desc Natural language description of the field
- :prefix String to prepend when formatting (defaults to field name)
- :format-fn Function to convert value to string (optional)"
  (let* ((name (plist-get plist :name))
         (input-fields (plist-get plist :input-fields))
         (output-fields (plist-get plist :output-fields)))
    ;; Basic validation
    (unless (stringp instructions)
      (error "Instructions must be a string"))
    
    ;; Generate default prefixes if needed
    (setq input-fields
          (mapcar (lambda (field)
                    (let* ((field-name (car field))
                           (field-plist (cdr field))
                           (prefix (plist-get field-plist :prefix)))
                      (unless prefix
                        (setq prefix (concat (capitalize 
                                              (replace-regexp-in-string
                                               "-" " " (symbol-name field-name)))
                                             ": "))
                        (setq field-plist (plist-put field-plist :prefix prefix)))
                      (cons field-name field-plist)))
                  input-fields))
    
    (setq output-fields
          (mapcar (lambda (field)
                    (let* ((field-name (car field))
                           (field-plist (cdr field))
                           (prefix (plist-get field-plist :prefix)))
                      (unless prefix
                        (setq prefix (concat (capitalize 
                                              (replace-regexp-in-string
                                               "-" " " (symbol-name field-name)))
                                             ": "))
                        (setq field-plist (plist-put field-plist :prefix prefix)))
                      (cons field-name field-plist)))
                  output-fields))
    
    (make-dsel-signature :name name
                         :instructions instructions
                         :input-fields input-fields
                         :output-fields output-fields)))

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
