;;; dsel-adapter.el --- Adapter between dsel and llm.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the adapter interface between dsel signatures and
;; llm.el providers, handling prompt formatting and output parsing.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'dsel-types)

(cl-defstruct dsel-adapter
  "Base adapter structure for bridging between dsel and llm.el.")

(cl-defstruct (dsel-default-chat-adapter (:include dsel-adapter))
  "Default chat adapter implementation for llm.el chat interfaces.")

(cl-defgeneric dsel-adapter-format-prompt (adapter signature demos current-inputs-alist)
  "Format a prompt for the given ADAPTER using SIGNATURE, DEMOS and CURRENT-INPUTS-ALIST.
Return an llm-chat-prompt structure.")

(cl-defgeneric dsel-adapter-parse-output (adapter signature llm-response-string)
  "Parse LLM-RESPONSE-STRING with the given ADAPTER and SIGNATURE.
Return an alist of (output-field-name . parsed-value).")

(defun dsel--format-field-description (field-plist)
  "Format a rich field description for FIELD-PLIST.
FIELD-PLIST is a property list with :name, :type, etc."
  (let* ((field-name (plist-get field-plist :name))
         (field-type (plist-get field-plist :type))
         (field-desc (plist-get field-plist :desc))
         (field-optional (plist-get field-plist :optional))
         (field-enum (plist-get field-plist :enum))
         (field-items (plist-get field-plist :items))
         (field-properties (plist-get field-plist :properties))
         (field-required (plist-get field-plist :required))
         (type-desc (format "%s%s"
                            (or field-type 'string)
                            (if field-optional " (optional)" ""))))
    ;; Build the description
    (concat 
     (format "- `%s` (%s): %s" field-name type-desc (or field-desc ""))

     ;; Add enum values if present
     (when (and field-enum (vectorp field-enum) (> (length field-enum) 0))
       (format "\n  Allowed values: %s" 
               (mapconcat #'identity 
                          (mapcar (lambda (i) (format "\"%s\"" (aref field-enum i)))
                                  (number-sequence 0 (1- (length field-enum))))
                          ", ")))

     ;; Add array item type if present
     (when (and (eq field-type 'array) field-items)
       (format "\n  Array items: %s" 
               (plist-get field-items :type)))

     ;; Add object properties if present (simplified)
     (when (and (eq field-type 'object) field-properties)
       (format "\n  Object with properties: %s" 
               (mapconcat (lambda (prop) (format "`%s`" (plist-get prop :name)))
                          field-properties ", "))))))

(cl-defmethod dsel-adapter-format-prompt ((adapter dsel-default-chat-adapter) 
                                          signature demos current-inputs-alist)
  "Format a chat prompt for the default adapter.
SIGNATURE is a `dsel-signature'.
DEMOS is a list of `dsel-example'.
CURRENT-INPUTS-ALIST is an alist of (field-name . value) for the current query."
  (let ((system-prompt
         (concat
          ;; Start with the signature instructions
          (dsel-signature-instructions signature)
          "\n\n"
          ;; Append descriptions for input fields
          "Your input fields are:\n"
          (mapconcat #'dsel--format-field-description
                     (dsel-signature-input-fields signature)
                     "\n")
          "\n\n"
          ;; Append descriptions for output fields
          "Your output fields are:\n"
          (mapconcat #'dsel--format-field-description
                     (dsel-signature-output-fields signature)
                     "\n")
          "\n\n"
          ;; Append formatting instruction
          "Please provide your response with each field clearly demarcated. For example:\n"
          (mapconcat
           (lambda (field-plist)
             (format "%s[Value for %s]" 
                     (plist-get field-plist :prefix)
                     (plist-get field-plist :name)))
           (dsel-signature-output-fields signature)
           "\n")))
        
        ;; Current input for main content argument
        (current-input-content 
         (dsel--format-input-fields signature current-inputs-alist)))

    ;; Create a proper llm-chat-prompt structure using expected keywords
    (llm-make-chat-prompt
     current-input-content
     :context system-prompt 
     :examples (cl-loop for demo in demos
                        collect (cons 
                                 (dsel--format-input-fields signature (dsel-example-inputs demo))
                                 (dsel--format-output-fields signature (dsel-example-labels demo)))))))

(defun dsel--format-input-fields (signature inputs-alist)
  "Format the INPUTS-ALIST according to the SIGNATURE's input field definitions."
  (let ((result ""))
    (dolist (field-plist (dsel-signature-input-fields signature))
      (let* ((field-name (plist-get field-plist :name))
             (field-prefix (plist-get field-plist :prefix))
             (format-fn (or (plist-get field-plist :format-fn) #'format))
             (input-pair (assq field-name inputs-alist)))
        (when input-pair
          (let ((value (cdr input-pair)))
            (setq result (concat result
                                 field-prefix
                                 (funcall format-fn "%s" value)
                                 "\n\n"))))))
    result))

(defun dsel--format-output-fields (signature outputs-alist)
  "Format the OUTPUTS-ALIST according to the SIGNATURE's output field definitions."
  (let ((result ""))
    (dolist (field-plist (dsel-signature-output-fields signature))
      (let* ((field-name (plist-get field-plist :name))
             (field-prefix (plist-get field-plist :prefix))
             (format-fn (or (plist-get field-plist :format-fn) #'format))
             (output-pair (assq field-name outputs-alist)))
        (when output-pair
          (let ((value (cdr output-pair)))
            (setq result (concat result
                                 field-prefix
                                 (funcall format-fn "%s" value)
                                 "\n\n"))))))
    result))

(cl-defmethod dsel-adapter-parse-output ((adapter dsel-default-chat-adapter)
                                         signature llm-response-string)
  "Parse LLM-RESPONSE-STRING using the default adapter and SIGNATURE."
  (let ((result nil))
    (dolist (field-plist (dsel-signature-output-fields signature))
      (let* ((field-name (plist-get field-plist :name))
             (field-prefix (plist-get field-plist :prefix))
             (field-optional (plist-get field-plist :optional))
             ;; Use regex to extract value between this prefix and the next prefix or end
             (prefix-pattern (regexp-quote field-prefix))
             ;; Use a simpler regex that matches until we see a double newline or end of string
             (value-pattern (concat prefix-pattern "\\(.*?\\)\\(?:\n\n\\|\n*\\'\\)"))
             (value-match (when (string-match value-pattern llm-response-string)
                            (match-string 1 llm-response-string)))
             (parsed-value (when value-match
                             (dsel--coerce-value (string-trim value-match) field-plist))))
        ;; Add to result if we got a value (nil is valid for booleans)
        (if parsed-value
            (push (cons field-name parsed-value) result)
          ;; Handle missing required fields
          (unless field-optional
            (message "Warning: Required field '%s' missing or failed to parse" field-name)))))
    (nreverse result)))

(defun dsel--coerce-value (string-value field-plist)
  "Coerce STRING-VALUE according to the type specified in FIELD-PLIST.
Handles enhanced field types including enum, array, and object."
  (let ((type (plist-get field-plist :type))
        (enum-values (plist-get field-plist :enum)))
    (cond
     ;; Handle enum values
     (enum-values
      (when (and (vectorp enum-values) (> (length enum-values) 0))
        (let ((trimmed-value (string-trim string-value)))
          (catch 'found
            (dotimes (i (length enum-values))
              (when (string= trimmed-value (aref enum-values i))
                (throw 'found trimmed-value)))
            ;; If we get here, no match was found
            (message "Warning: Value '%s' not in enum %s" trimmed-value enum-values)
            trimmed-value))))
     
     ;; Handle basic types
     ((eq type 'string) string-value)
     ((eq type 'integer) (string-to-number string-value))
     ((eq type 'number) (string-to-number string-value))
     ((eq type 'boolean)
      (cond
       ((string-match-p "\\`\\(?:t\\|true\\|yes\\)\\'" (downcase string-value)) t)
       ((string-match-p "\\`\\(?:nil\\|false\\|no\\)\\'" (downcase string-value)) nil)
       (t nil)))
     
     ;; Handle complex types
     ((eq type 'array)
      (condition-case nil
          (json-parse-string string-value)
        (error
         ;; Fallback: try to parse as a comma-separated list
         (mapcar #'string-trim (split-string string-value "," t)))))
     
     ((eq type 'object)
      (condition-case nil
          (json-parse-string string-value :object-type 'alist)
        (error
         ;; Return the raw string if we can't parse as JSON
         string-value)))
     
     ;; Default fallback
     (t string-value))))

(provide 'dsel-adapter)
;;; dsel-adapter.el ends here
