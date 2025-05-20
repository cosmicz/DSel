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

(cl-defmethod dsel-adapter-format-prompt ((adapter dsel-default-chat-adapter) 
                                          signature demos current-inputs-alist)
  "Format a chat prompt for the default adapter.
SIGNATURE is a `dsel-signature'.
DEMOS is a list of `dsel-example'.
CURRENT-INPUTS-ALIST is an alist of (field-name . value) for the current query."
  (let* ((system-prompt
          (concat
           ;; Start with the signature instructions
           (dsel-signature-instructions signature)
           "\n\n"
           ;; Append descriptions for input fields
           "Your input fields are:\n"
           (mapconcat
            (lambda (field-pair)
              (let* ((field-name (car field-pair))
                     (field-plist (cdr field-pair))
                     (field-type (plist-get field-plist :type))
                     (field-desc (plist-get field-plist :desc)))
                (format "- `%s` (%s): %s"
                        field-name
                        (or field-type 'string)
                        (or field-desc ""))))
            (dsel-signature-input-fields signature)
            "\n")
           "\n\n"
           ;; Append descriptions for output fields
           "Your output fields are:\n"
           (mapconcat
            (lambda (field-pair)
              (let* ((field-name (car field-pair))
                     (field-plist (cdr field-pair))
                     (field-type (plist-get field-plist :type))
                     (field-desc (plist-get field-plist :desc)))
                (format "- `%s` (%s): %s"
                        field-name
                        (or field-type 'string)
                        (or field-desc ""))))
            (dsel-signature-output-fields signature)
            "\n")
           "\n\n"
           ;; Append formatting instruction
           "Please provide your response with each field clearly demarcated. For example:\n"
           (mapconcat
            (lambda (field-pair)
              (let* ((field-name (car field-pair))
                     (field-plist (cdr field-pair))
                     (field-prefix (plist-get field-plist :prefix)))
                (format "%s[Value for %s]" field-prefix field-name)))
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
    (dolist (field-pair (dsel-signature-input-fields signature))
      (let* ((field-name (car field-pair))
             (field-plist (cdr field-pair))
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
    (dolist (field-pair (dsel-signature-output-fields signature))
      (let* ((field-name (car field-pair))
             (field-plist (cdr field-pair))
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
    (dolist (field-pair (dsel-signature-output-fields signature))
      (let* ((field-name (car field-pair))
             (field-plist (cdr field-pair))
             (field-prefix (plist-get field-plist :prefix))
             (field-type (plist-get field-plist :type))
             ;; Use regex to extract value between this prefix and the next prefix or end
             (prefix-pattern (regexp-quote field-prefix))
             (value-pattern (concat prefix-pattern "\\(.*?\\)\\(?:\n\\|\\'\\)"))
             (value-match (when (string-match value-pattern llm-response-string)
                            (match-string 1 llm-response-string)))
             (parsed-value (when value-match
                             (dsel--coerce-value (string-trim value-match) field-type))))
        (when parsed-value
          (push (cons field-name parsed-value) result))))
    (nreverse result)))

(defun dsel--coerce-value (string-value type)
  "Coerce STRING-VALUE to the specified TYPE."
  (pcase type
    ('string string-value)
    ('integer (string-to-number string-value))
    ('number (string-to-number string-value))
    ('boolean (cond
               ((string-match-p "\\`\\(?:t\\|true\\|yes\\)\\'" 
                                (downcase string-value)) t)
               ((string-match-p "\\`\\(?:nil\\|false\\|no\\)\\'"
                                (downcase string-value)) nil)
               (t nil)))
    (_ string-value)))  ; Default to string for unknown types

(provide 'dsel-adapter)
;;; dsel-adapter.el ends here
