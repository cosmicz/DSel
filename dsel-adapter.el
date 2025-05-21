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
(require 'json)

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
                                          signature demos current-inputs-alist)
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
             (format "%s[Value for %s]"
                     (plist-get field-plist :prefix)
                     (plist-get field-plist :name)))
           (dsel-signature-output-fields signature)
           "\n")))
        (current-input-content
         (dsel--format-input-fields signature current-inputs-alist)))
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

;; In dsel-adapter.el

(cl-defmethod dsel-adapter-parse-output ((adapter dsel-default-chat-adapter)
                                         signature llm-response-string)
  "Parse LLM-RESPONSE-STRING using the default adapter and SIGNATURE.
This version attempts to handle multi-line fields more robustly."
  (let ((parsed-fields-alist nil) ; Alist of (field-name . raw-string-value)
        (output-field-plists (dsel-signature-output-fields signature))
        (current-pos 0))

    ;; 1. Create a list of (prefix-string . field-name-symbol) for quick lookup
    ;;    Sort them by length descending to handle overlapping prefixes (e.g., "Note:" and "Note Details:")
    (let* ((prefix-map
            (sort (mapcar (lambda (fp)
                            (cons (plist-get fp :prefix) (plist-get fp :name)))
                          output-field-plists)
                  (lambda (a b) (> (length (car a)) (length (car b))))))
           (all-prefixes (mapcar #'car prefix-map)))

      ;; 2. Iteratively find and extract fields
      (while (< current-pos (length llm-response-string))
        (let* ((next-match-data nil) ; To store (match-start field-name prefix-len)
               (search-from current-pos))

          ;; Find the earliest occurrence of any known prefix from current-pos
          (dolist (prefix-info prefix-map)
            (let* ((prefix-str (car prefix-info))
                   (field-name-for-prefix (cdr prefix-info))
                   (match-start (string-match (regexp-quote prefix-str) llm-response-string search-from)))
              (when match-start
                (if (or (null next-match-data) (< match-start (car next-match-data)))
                    (setq next-match-data (list match-start field-name-for-prefix (length prefix-str)))))))

          (if next-match-data
              (let* ((field-start-pos (car next-match-data))
                     (field-name (cadr next-match-data))
                     (prefix-len (caddr next-match-data))
                     (value-start-pos (+ field-start-pos prefix-len))
                     (value-end-pos (length llm-response-string)) ; Default to end of string
                     (raw-value nil))

                ;; If there was a previous field, its value ends where this one starts
                (when (and parsed-fields-alist
                           (null (assoc field-name parsed-fields-alist))) ; Avoid re-processing due to unordered LLM output
                  ;; This logic is complex if LLM reorders fields.
                  ;; For now, assume this means we found the *next* field for the previous one.
                  ;; A simpler model: the value of a field is from its prefix to the next known prefix or EOS.
                  )


                ;; Find the end of the current field's value
                ;; It ends either at the start of the *next* different prefix, or end of string
                (let ((next-value-search-start value-start-pos))
                  (dolist (next-prefix-info prefix-map)
                    (let* ((next-prefix-str (car next-prefix-info))
                           (next-field-name (cdr next-prefix-info)))
                      ;; Only consider it a delimiter if it's a *different* field's prefix
                      (unless (eq field-name next-field-name)
                        (let ((next-prefix-match-pos (string-match (regexp-quote next-prefix-str)
                                                                   llm-response-string
                                                                   next-value-search-start)))
                          (when next-prefix-match-pos
                            (setq value-end-pos (min value-end-pos next-prefix-match-pos))))))))

                (setq raw-value (string-trim (substring llm-response-string value-start-pos value-end-pos)))
                (push (cons field-name raw-value) parsed-fields-alist)

                ;; Advance current-pos to the end of this extracted field's value
                ;; This is where it gets tricky if we want to re-scan for out-of-order fields.
                ;; A simpler model for now: advance past this found field.
                (setq current-pos value-end-pos)

                ;; If we set current_pos to value_end_pos, and value_end_pos was determined by the start
                ;; of the *next* prefix, the next loop iteration will re-find that next prefix.
                ;; If value_end_pos was (length llm-response-string), the loop terminates.
                )
            (setq current-pos (length llm-response-string)) ; No more known prefixes found
            )))
      ) ; End of while and outer let*

    ;; 3. Coerce and validate based on signature
    (let ((final-result nil))
      (dolist (field-plist output-field-plists)
        (let* ((field-name (plist-get field-plist :name))
               (field-optional (plist-get field-plist :optional))
               (raw-value-pair (assq field-name parsed-fields-alist))
               (raw-value (if raw-value-pair (cdr raw-value-pair) nil)))

          (if raw-value
              (let ((coerced-value (dsel--coerce-value raw-value field-plist)))
                (push (cons field-name coerced-value) final-result))
            (unless field-optional
              (message "Warning: Required output field '%s' was not found in LLM response." field-name)))))
      (nreverse final-result))))

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
      (let ((num (string-to-number string-value)))
        ;; Check if conversion worked - string-to-number returns 0 for invalid input
        (if (or (string-match-p "^\\s*0+\\s*$" string-value)  ; It's actually "0"
                (not (zerop num)))                            ; Or conversion worked
            num
          (if (plist-get field-plist :name)
              (error "Invalid integer format '%s' for field '%s'"
                     string-value (plist-get field-plist :name))
            string-value))))
     ((eq type 'number)
      (let ((num (string-to-number string-value)))
        ;; Check if conversion worked - string-to-number returns 0 for invalid input
        (if (or (string-match-p "^\\s*0+\\s*$" string-value)  ; It's actually "0"
                (not (zerop num)))                            ; Or conversion worked
            num
          (if (plist-get field-plist :name)
              (error "Invalid number format '%s' for field '%s'"
                     string-value (plist-get field-plist :name))
            string-value))))
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
