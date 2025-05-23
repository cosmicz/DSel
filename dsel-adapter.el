;;; dsel-adapter.el --- Adapter between dsel and llm.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the adapter interface between dsel signatures and
;; llm.el providers, handling prompt formatting and output parsing.

;;; Code:

(require 'cl-lib)
(require 'llm)
(require 'dsel-types)
(require 'dsel-settings)
(require 'json)

(cl-defstruct dsel-adapter
  "Base adapter structure for bridging between dsel and llm.el.")

(cl-defstruct (dsel-default-chat-adapter (:include dsel-adapter))
  "Default chat adapter implementation for llm.el chat interfaces.")

(cl-defgeneric dsel-adapter-format-prompt (adapter signature demos current-inputs-alist &optional config)
  "Format a prompt for the given ADAPTER using SIGNATURE, DEMOS and CURRENT-INPUTS-ALIST.
CONFIG is an optional plist with LLM-specific configuration options.
Return an llm-chat-prompt structure.")

(cl-defgeneric dsel-adapter-parse-output (adapter signature llm-response-string)
  "Parse LLM-RESPONSE-STRING with the given ADAPTER and SIGNATURE.
Return a list of field result plists, each with :name, :value, and :error keys.")

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
         (type-desc (format "%s%s"
                            (or field-type 'string)
                            (if field-optional " (optional)" ""))))
    (concat
     (format "- `%s` (%s): %s" field-name type-desc (or field-desc ""))
     (when (and field-enum (vectorp field-enum) (> (length field-enum) 0))
       (format "\n  Allowed values: %s"
               (mapconcat (lambda (val) (format "\"%s\"" val)) (append field-enum nil) ", ")))
     (when (and (eq field-type 'array) field-items (plist-get field-items :type))
       (format "\n  Array items are of type: %s" (plist-get field-items :type)))
     (when (and (eq field-type 'object) field-properties)
       (format "\n  Object with properties: %s"
               (mapconcat (lambda (prop-plist) (format "`%s` (%s)"
                                                       (plist-get prop-plist :name)
                                                       (plist-get prop-plist :type)))
                          field-properties ", "))))))

(cl-defmethod dsel-adapter-format-prompt ((adapter dsel-default-chat-adapter)
                                          signature demos current-inputs-alist &optional config)
  "Format a chat prompt for the default adapter.
SIGNATURE is a `dsel-signature'.
DEMOS is a list of `dsel-example'.
CURRENT-INPUTS-ALIST is an alist of (field-name . value) for the current query."
  (let ((system-prompt
         (concat
          (dsel-signature-instructions signature)
          "\n\n"
          "Your input fields are:\n"
          (mapconcat #'dsel--format-field-description
                     (dsel-signature-input-fields signature)
                     "\n")
          "\n\n"
          "Your output fields are:\n"
          (mapconcat #'dsel--format-field-description
                     (dsel-signature-output-fields signature)
                     "\n")
          "\n\n"
          "Please provide your response with each field clearly demarcated. For example:\n"
          (mapconcat
           (lambda (field-plist)
             (format "%s [Value for %s]"
                     (plist-get field-plist :prefix)
                     (plist-get field-plist :name)))
           (dsel-signature-output-fields signature)
           "\n")))
        (current-input-content
         (dsel--format-input-fields signature current-inputs-alist)))
    (apply #'llm-make-chat-prompt
           current-input-content
           :context system-prompt
           :examples (cl-loop for demo in demos
                              collect (cons
                                       (dsel--format-input-fields signature (dsel-example-inputs demo))
                                       (dsel--format-output-fields signature (dsel-example-labels demo))))
           ;; Add any provided config options as additional keyword args
           (when config
             (cl-loop for (key value) on config by #'cddr
                      collect key
                      collect value)))))

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
                                 (funcall format-fn "%s" (if (stringp value) value (format "%S" value)))
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
                                 (funcall format-fn "%s" (if (stringp value) value (format "%S" value)))
                                 "\n\n"))))))
    result))

(defun dsel--find-earliest-matching-prefix (text start-pos output-field-plists)
  "Find the earliest occurring field from OUTPUT-FIELD-PLISTS in TEXT at or after START-POS.
Returns (list MATCHING-FIELD-PLIST PREFIX-START-INDEX ACTUAL-PREFIX-END-INDEX) or nil."
  (let ((best-match-field-plist nil)
        (earliest-match-start-idx -1) ; Stores the (match-beginning 0) of the earliest prefix
        (best-match-prefix-actual-end -1)) ; Stores the (match-end 0) of the regex for that prefix
    (dolist (field-plist output-field-plists)
      (let* ((prefix-from-sig (plist-get field-plist :prefix))
             ;; Ensure prefix isn't empty, which would cause issues with regexp-quote and matching
             (_ (when (string-empty-p prefix-from-sig) (error "Empty prefix found for field %s" (plist-get field-plist :name))))
             (regex-to-find-prefix (concat (regexp-quote prefix-from-sig) "\\s-*"))
             (match-start (string-match regex-to-find-prefix text start-pos)))
        (when match-start
          (if (or (= earliest-match-start-idx -1) (< match-start earliest-match-start-idx))
              (setq earliest-match-start-idx match-start
                    best-match-field-plist field-plist
                    best-match-prefix-actual-end (match-end 0)))))) ; This is (match-end 0) of the regex, i.e., after prefix and spaces
    (if best-match-field-plist
        (list best-match-field-plist earliest-match-start-idx best-match-prefix-actual-end)
      nil)))

(defun dsel--extract-raw-value-and-next-pos (text current-field-plist value-start-idx all-output-field-plists)
  "Extract raw string value for CURRENT-FIELD-PLIST starting at VALUE-START-IDX.
Value ends before the next known prefix or at end of TEXT.
Returns (list RAW-VALUE-STRING NEW-POS-AFTER-VALUE)."
  (let ((value-end-idx (length text)) ; Default to end of string
        raw-value)
    ;; Find where this field's value ends
    (let ((next-known-prefix-earliest-start-pos -1))
      (dolist (next-candidate-plist all-output-field-plists)
        (unless (eq current-field-plist next-candidate-plist)
          (let* ((next-prefix-from-sig (plist-get next-candidate-plist :prefix))
                 (next-regex (concat (regexp-quote next-prefix-from-sig) "\\s-*"))
                 (match-pos (string-match next-regex text value-start-idx)))
            (when match-pos
              (if (or (= next-known-prefix-earliest-start-pos -1) (< match-pos next-known-prefix-earliest-start-pos))
                  (setq next-known-prefix-earliest-start-pos match-pos))))))
      (when (/= next-known-prefix-earliest-start-pos -1)
        (setq value-end-idx next-known-prefix-earliest-start-pos)))

    (setq raw-value (if (>= value-start-idx value-end-idx)
                        ""
                      (string-trim (substring text value-start-idx value-end-idx))))
    (list raw-value value-end-idx)))

(cl-defmethod dsel-adapter-parse-output ((adapter dsel-default-chat-adapter)
                                         signature llm-response-string)
  "Parse LLM-RESPONSE-STRING using the default adapter and SIGNATURE."
  (let ((raw-parsed-fields (make-hash-table :test 'eq))
        (output-field-plists (dsel-signature-output-fields signature))
        (current-pos 0)
        (loop-count 0))

    (dsel--log 'debug "PARSE-OUTPUT: START. Response length: %d. Response: %S" (length llm-response-string) llm-response-string)

    (while (< current-pos (length llm-response-string))
      (setq loop-count (1+ loop-count))
      (when (> loop-count (+ 5 (* 2 (length output-field-plists))))
        (dsel--log 'error "PARSE-OUTPUT: ERROR - Loop guard hit (%d loops). Pos: %d. Aborting." loop-count current-pos)
        (error "Parser loop stuck (guard hit)")
        (cl-return)) ; Should not be reached due to error

      (dsel--log 'debug "PARSE-OUTPUT: WHILE iter #%d, current_pos: %d" loop-count current-pos)
      (let ((match-info (dsel--find-earliest-matching-prefix llm-response-string current-pos output-field-plists)))
        (if match-info
            (let* ((matched-field-plist (nth 0 match-info))
                   (prefix-start-idx (nth 1 match-info))
                   (actual-prefix-end-idx (nth 2 match-info))
                   (field-name (plist-get matched-field-plist :name))
                   extraction-result raw-value new-pos)

              (dsel--log 'debug "PARSE-OUTPUT: Found field '%s' starting at index %d (prefix ends at %d)."
                         field-name prefix-start-idx actual-prefix-end-idx)

              ;; If prefix isn't at current-pos, there's unparsed text. For now, we skip it and advance.
              ;; A more robust parser might handle this "inter-field" text.
              (when (> prefix-start-idx current-pos)
                (dsel--log 'debug "PARSE-OUTPUT: Skipping unparsed text from %d to %d: '%s'"
                           current-pos prefix-start-idx
                           (substring llm-response-string current-pos prefix-start-idx))
                (setq current-pos prefix-start-idx))


              (setq extraction-result (dsel--extract-raw-value-and-next-pos
                                       llm-response-string
                                       matched-field-plist
                                       actual-prefix-end-idx ; value_start_idx
                                       output-field-plists))
              (setq raw-value (car extraction-result)
                    new-pos (cadr extraction-result))

              (dsel--log 'debug "PARSE-OUTPUT: Field '%s' raw value: %S. Next search will start at: %d"
                         field-name raw-value new-pos)
              (puthash field-name raw-value raw-parsed-fields)
              (setq current-pos new-pos))

          ;; ELSE: No more known prefixes found
          (progn
            (dsel--log 'debug "PARSE-OUTPUT: No more known prefixes found from pos %d. Exiting WHILE loop." current-pos)
            (setq current-pos (length llm-response-string)))))) ; Force loop termination

    (dsel--log 'debug "PARSE-OUTPUT: Loop finished. Final current_pos: %d. Parsed intermediate: %S" current-pos raw-parsed-fields)

    ;; Pass 2: Coerce values and handle optional/required, returning field result plists
    (let ((field-results nil))
      (dolist (field-plist output-field-plists)
        (let* ((field-name (plist-get field-plist :name))
               (field-type (plist-get field-plist :type))
               (is-optional (plist-get field-plist :optional))
               ;; Use a unique sentinel to distinguish "not found" from "found with nil value"
               (raw-value (gethash field-name raw-parsed-fields :_dsel_field_not_found_))
               (current-field-value nil)
               (current-field-error nil))

          (if (not (eq raw-value :_dsel_field_not_found_))
              ;; Field's prefix was found in the LLM response
              (progn
                ;; Attempt to coerce the raw string value
                (condition-case err
                    (setq current-field-value (dsel--coerce-value raw-value field-plist))
                  (error
                   ;; Coercion failed
                   (setq current-field-value nil
                         current-field-error (list :type :coercion
                                                   :message (error-message-string err)
                                                   :raw-value raw-value
                                                   :field field-name))))

                ;; Check for required field empty error (only if no coercion error yet)
                (when (and (null current-field-error)
                           (null current-field-value)
                           (not is-optional)
                           (not (eq field-type 'boolean)))
                  (setq current-field-error (list :type :required-field-empty
                                                  :message (format "Required field '%s' was empty, resulting in nil" field-name)
                                                  :field field-name)))

                ;; Add field result to list (but omit optional fields with nil values and no errors)
                (unless (and (null current-field-error)
                             (null current-field-value)
                             is-optional
                             (not (eq field-type 'boolean)))
                  (push (list :name field-name
                             :value current-field-value
                             :error current-field-error)
                        field-results)))

            ;; Field's prefix was NOT found in the LLM response
            (if (not is-optional)
                ;; Required field missing
                (push (list :name field-name
                           :value nil
                           :error (list :type :missing-required
                                       :message (format "Required field '%s' not found in LLM response" field-name)
                                       :field field-name))
                      field-results)
              ;; Optional field missing - omit from results (don't add to field-results)
              ))))

      (nreverse field-results))))

(cl-defun dsel--coerce-value (string-value field-plist)
  "Coerce STRING-VALUE to the type specified in FIELD-PLIST.
Returns the coerced value.
Signals an error for invalid formats, enum mismatches, or unrecognized boolean values.

Behavior for empty input strings (`trimmed-value` being `\"\"`):
- `:type string`: returns `\"\"`.
- `:type integer`, `:type number`: returns `nil` (representing 'no value provided').
- `:type boolean`: **errors**, as empty string is not a recognized boolean.
- `:type array`, `:type object`: returns `nil` (representing 'no value provided').

Type coercion details:
- `:type string`: Returns the trimmed string.
- `:type integer` / `:type number`: Parses the string. Returns `nil` for empty strings.
  Errors on invalid numeric formats or non-integer values for `integer` type.
- `:type boolean`: Parses recognized true/false strings (case-insensitive).
  Errors for any other input, including empty strings.
- `:type array`: Parses JSON arrays or, as fallback, comma-separated values.
  Returns `nil` for empty strings. Errors if JSON is invalid or not an array.
- `:type object`: Parses JSON objects. Returns `nil` for empty strings.
  Errors if JSON is invalid or not an object (alist).
- `:enum [...]`: Validates `trimmed-value` against string representations of enum options.
  If valid, `trimmed-value` is then coerced per its `:type`. Errors if not in enum.
- Unknown types: Logs warning, returns trimmed string."
  (let* ((field-name (plist-get field-plist :name))
         (original-string-value string-value) ; Keep for error messages
         (trimmed-value (if string-value (string-trim string-value) ""))
         (type (plist-get field-plist :type))
         (enum-values (plist-get field-plist :enum)))

    ;; 1. Handle original string-value being nil (not just empty after trim)
    (when (null string-value)
      (cl-return-from dsel--coerce-value
        (pcase type
          ((or 'integer 'number 'array 'object) nil)
          ('boolean (error "Cannot coerce nil to boolean for field '%s'. Expected a string" field-name)) ; Nil string is invalid for boolean
          ('string "") ; A nil input string becomes an empty string for type string.
          (_ "")))) ; Default for other unknown types if string-value itself is nil

    ;; 2. Enum Validation (if enum is present)
    (when enum-values
      (unless (and (vectorp enum-values)
                   (cl-find-if (lambda (allowed-val)
                                 (string= trimmed-value
                                          (if (stringp allowed-val)
                                              allowed-val
                                            (format "%s" allowed-val))))
                               enum-values))
        (error "Value '%s' (from input '%s') not in enum %s for field '%s'"
               trimmed-value original-string-value enum-values field-name)))
    ;; If execution reaches here, enum validation passed or no enum.
    ;; `trimmed-value` is the string to be coerced.

    ;; 3. Regular type coercion
    (pcase type
      ('string
       trimmed-value)

      ((or 'integer 'number)
       (if (string-empty-p trimmed-value)
           nil
         (let ((num (string-to-number trimmed-value)))
           (if (and (zerop num) (not (string-match-p "\\`[+-]?0\\(?:\\.0*\\)?\\'" trimmed-value)))
               (error "Invalid %s format '%s' (from input '%s') for field '%s'"
                      type trimmed-value original-string-value field-name)
             (if (and (eq type 'integer) (floatp num) (/= num (truncate num)))
                 (error "Non-integer number %S (from input '%s') received for integer field '%s'"
                        num original-string-value field-name)
               num)))))

      ('boolean
       (let ((lower-trimmed (downcase trimmed-value)))
         (cond
          ((member lower-trimmed '("t" "true" "yes" "1")) t)
          ((member lower-trimmed '("nil" "false" "no" "0")) nil)
          (t (error "Invalid boolean value '%s' (from input '%s') for field '%s'. Expected true/false/yes/no/t/nil/0/1 or their synonyms"
                    trimmed-value original-string-value field-name)))))

      ('array
       (if (string-empty-p trimmed-value)
           nil
         (let (parsed-val)
           (condition-case err
               (progn
                 (setq parsed-val (json-read-from-string trimmed-value))
                 (unless (or (vectorp parsed-val) (listp parsed-val))
                   (error "Parsed JSON value for field '%s' (from input '%s') is not an array structure: %S"
                          field-name original-string-value parsed-val))
                 parsed-val)
             (json-error
              (if (string-match-p "," trimmed-value)
                  (mapcar #'string-trim (split-string trimmed-value "," t))
                (error "Invalid array format for field '%s' (from input '%s'). Not valid JSON ('%s') and not comma-separated"
                       field-name original-string-value (error-message-string err))))
             (error
              (error "Error parsing array for field '%s' (from input '%s'): %s"
                     field-name original-string-value (error-message-string err)))))))

      ('object
       (if (string-empty-p trimmed-value)
           nil
         (let (parsed-val)
           (condition-case err
               (progn
                 (setq parsed-val (json-read-from-string trimmed-value))
                 (unless (and (listp parsed-val) (or (null parsed-val) (consp (car parsed-val))))
                   (error "Parsed JSON value for field '%s' (from input '%s') is not an object structure (alist): %S"
                          field-name original-string-value parsed-val))
                 parsed-val)
             (json-error
              (error "Invalid object format for field '%s' (from input '%s'). Not valid JSON: '%s'"
                     field-name original-string-value (error-message-string err)))
             (error
              (error "Error parsing object for field '%s' (from input '%s'): %s"
                     field-name original-string-value (error-message-string err)))))))

      (_
       (dsel--log 'warning "Coercing field '%s': Unknown type '%s'. Returning trimmed string value: \"%s\" (from input \"%s\")"
                  field-name type trimmed-value original-string-value)
       trimmed-value))))

(provide 'dsel-adapter)
;;; dsel-adapter.el ends here
