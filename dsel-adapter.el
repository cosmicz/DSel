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
        (final-result-alist nil)
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

    ;; Pass 2: Coerce values and handle optional/required
    (dolist (field-plist output-field-plists)
      (let* ((field-name (plist-get field-plist :name))
             (field-optional (plist-get field-plist :optional))
             (raw-value (gethash field-name raw-parsed-fields)))
        (if raw-value
            (let ((coerced-value (dsel--coerce-value raw-value field-plist)))
              (if (or coerced-value (eq coerced-value t) (eq coerced-value nil) (stringp coerced-value))
                  (push (cons field-name coerced-value) final-result-alist)
                (dsel--log 'warning "PARSE-OUTPUT: Coerced value for '%s' was nil and not added (type: %s, raw: %S)"
                           field-name (plist-get field-plist :type) raw-value)))
          (unless field-optional
            (error "Required output field '%s' was not found in LLM response" field-name)))))
    (nreverse final-result-alist)))

(defun dsel--coerce-value (string-value field-plist)
  "Coerce STRING-VALUE according to the type specified in FIELD-PLIST.
Handles enhanced field types including enum, array, and object."
  (let ((type (plist-get field-plist :type))
        (enum-values (plist-get field-plist :enum)))
    (cond
     (enum-values
      (let ((trimmed-value (string-trim string-value)))
        (if (and (vectorp enum-values) (cl-find trimmed-value enum-values :test #'string=))
            trimmed-value
          (error "Value '%s' not in enum %s for field '%s'"
                 trimmed-value enum-values (plist-get field-plist :name)))))
     ((eq type 'string) string-value)
     ((eq type 'integer)
      (let ((trimmed (string-trim string-value)))
        (cond
         ;; Empty string - return it as-is for empty fields
         ((string= "" trimmed) trimmed)
         (t (let ((num (string-to-number trimmed)))
              ;; Check if conversion worked - string-to-number returns 0 for invalid input
              (if (or (string-match-p "^\\s*0+\\s*$" trimmed)  ; It's actually "0"
                      (not (zerop num)))                       ; Or conversion worked
                  num
                (if (plist-get field-plist :name)
                    (error "Invalid integer format '%s' for field '%s'"
                           trimmed (plist-get field-plist :name))
                  trimmed)))))))
     ((eq type 'number)
      (let ((trimmed (string-trim string-value)))
        (cond
         ;; Empty string - return it as-is for empty fields
         ((string= "" trimmed) trimmed)
         (t (let ((num (string-to-number trimmed)))
              ;; Check if conversion worked - string-to-number returns 0 for invalid input
              (if (or (string-match-p "^\\s*0+\\s*$" trimmed)  ; It's actually "0"
                      (not (zerop num)))                       ; Or conversion worked
                  num
                (if (plist-get field-plist :name)
                    (error "Invalid number format '%s' for field '%s'"
                           trimmed (plist-get field-plist :name))
                  trimmed)))))))
     ((eq type 'boolean)
      (cond
       ((string-match-p "\\`\\(?:t\\|true\\|yes\\)\\'" (downcase string-value)) t)
       ((string-match-p "\\`\\(?:nil\\|false\\|no\\)\\'" (downcase string-value)) nil)
       (t nil)))
     ((eq type 'array)
      (condition-case err
          (json-read-from-string string-value) ; Use json-read for vectors
        (error
         ;; Try comma-separated format before giving up
         (if (string-match-p "," string-value)
             (mapcar #'string-trim (split-string string-value "," t))
           (error "Invalid array format '%s' for field '%s': %s"
                  string-value (plist-get field-plist :name) (error-message-string err))))))
     ((eq type 'object)
      (condition-case err
          (json-read-from-string string-value) ; json-read uses alist for objects
        (error
         (error "Invalid object format '%s' for field '%s': %s"
                string-value (plist-get field-plist :name) (error-message-string err)))))
     (t string-value))))

(provide 'dsel-adapter)
;;; dsel-adapter.el ends here
