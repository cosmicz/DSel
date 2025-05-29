;;; dsel-module.el --- Module base for dsel  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Cosmin-Octavian C. (cosmicz)
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the module base for dsel, which is used to build
;; complex LLM applications by composing simpler modules.

;;; Code:

(require 'cl-lib)
(require 'dsel-settings)  ; For dsel--log
(require 'dsel-types)     ; For dsel-make-prediction

(cl-defstruct dsel-module
  "Base module structure for building LLM application components."
  name                                  ; Symbol: name of the module instance
  (submodules nil :type list)           ; Alist: (name . module) pairs for child modules/predictors
  compiled-p)                           ; Boolean: if the module has been optimized

(cl-defgeneric dsel-aforward (module &rest kwargs)
  "Asynchronously execute MODULE with KWARGS.
Returns a `dsel-aio-promise' that resolves to a `dsel-prediction' object.
The `dsel-prediction' object's `.errors' slot should be checked by the caller,
as the promise will typically resolve even if operational errors occur,
placing error details in the prediction object.
Critical internal errors or unhandled conditions might cause promise rejection.")

(cl-defgeneric dsel-forward (module &rest kwargs)
  "Execute MODULE with KWARGS and return a prediction.
This is the synchronous execution method for modules.")

(cl-defgeneric dsel-collect-predictors (module)
  "Recursively collect all active predictor instances from MODULE and its sub-modules.")

(cl-defmethod dsel-collect-predictors ((module dsel-module))
  "Recursively collect all predictor instances from MODULE and its sub-modules."
  (let ((collected '()))
    ;; Iterate over direct children in the 'submodules' slot (now an alist)
    (dolist (child-pair (dsel-module-submodules module))
      (let ((child (cdr child-pair)))
        (if (and (fboundp 'dsel-predict-p) (dsel-predict-p child))
            (push child collected)
          (when (dsel-module-p child)
            (setq collected (append (dsel-collect-predictors child) collected))))))
    (nreverse collected)))

(cl-defgeneric dsel-module-get-submodule (module name)
  "Get a submodule from MODULE by NAME.
Returns nil if no submodule with NAME is found.")

(cl-defmethod dsel-module-get-submodule ((module dsel-module) name)
  "Get a submodule from MODULE by NAME.
Returns nil if no submodule with NAME is found."
  (cdr (assq name (dsel-module-submodules module))))

(cl-defgeneric dsel-module-named-predictors (module)
  "Return an alist of (name . predict-instance) within MODULE.
This finds named predictors at any depth.")

(cl-defmethod dsel-module-named-predictors ((module dsel-module))
  "Return an alist of (name . predict-instance) within MODULE."
  (let ((result nil))
    (dolist (predictor (dsel-collect-predictors module) result)
      (when (dsel-module-name predictor)
        (push (cons (dsel-module-name predictor) predictor) result)))
    (nreverse result)))

(cl-defgeneric dsel-module-reset-optimizable-state (module)
  "Generic function to reset optimizer-specific state on a module.")

(cl-defmethod dsel-module-reset-optimizable-state ((module dsel-module))
  "Base method: Resets 'compiled-p to nil."
  (setf (dsel-module-compiled-p module) nil))

(cl-defmethod dsel-module-reset-copy ((module dsel-module))
  "Return a new copy of MODULE with its optimizable state reset."
  (let ((copy (dsel-module-deepcopy module)))
    (dolist (m (dsel-collect-predictors copy))
      (dsel-module-reset-optimizable-state m))
    ;; Also reset the top-level module itself if it's not a predictor
    (unless (and (fboundp 'dsel-predict-p) (dsel-predict-p copy))
      (dsel-module-reset-optimizable-state copy))
    copy))

(cl-defgeneric dsel-module-deepcopy (module)
  "Return a deep copy of MODULE.")

(cl-defmethod dsel-module-deepcopy ((module dsel-module))
  "Return a deep copy of MODULE."
  (let* ((copy (copy-dsel-module module))
         ;; Deep copy submodules recursively (now from alist)
         (submodule-copies
          (mapcar (lambda (submodule-pair)
                    (let ((submodule-name (car submodule-pair))
                          (submodule (cdr submodule-pair)))
                      (cons submodule-name
                            (if (dsel-module-p submodule)
                                (dsel-module-deepcopy submodule)
                              ;; For non-module objects, use a simple copy
                              (copy-sequence submodule)))))
                  (dsel-module-submodules module))))
    (setf (dsel-module-submodules copy) submodule-copies)
    copy))

;;; Async/Sync bridge and helper functions

(defun dsel--kwargs-to-plist (kwargs)
  "Convert KWARGS (list of alternating keys and values) to a plist.
Helper function for error handling in sync wrapper methods."
  (let ((result nil)
        (remaining kwargs))
    (while remaining
      (let ((key (pop remaining))
            (value (when remaining (pop remaining))))
        (when key
          (setq result (append result (list key value))))))
    result))

(cl-defmethod dsel-forward ((module dsel-module) &rest kwargs)
  "Default implementation of synchronous forward execution.
This is a convenience wrapper around `dsel-aforward' using `dsel-aio-wait-for'."
  (condition-case err
      (dsel-aio-wait-for (apply #'dsel-aforward module kwargs))
    (dsel-aio-timeout
     (dsel--log 'error "DSel sync call timed out for %S: %S" module err)
     (apply #'dsel-make-prediction
            (append (dsel--kwargs-to-plist kwargs)
                    (list :errors `((:type :timeout :message "Synchronous call timed out"))))))
    ;; Catch other Elisp errors that dsel-aio-wait-for might re-signal from a rejection
    (error
     (dsel--log 'error "Error in DSel sync call for %S: %S" module err)
     (apply #'dsel-make-prediction
            (append (dsel--kwargs-to-plist kwargs)
                    (list :errors `((:type :sync-wrapper-error
                                           :message ,(error-message-string err)
                                           :original-error ,err))))))))

;;; Definition macros for modules

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
  - Additional SLOT specs as in `cl-defstruct': (SLOT [DEFAULT] [:keyword VALUE]...)

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

(cl-defmacro dsel-defaforward (module-name arg-list &optional docstring &body body)
  "Define an asynchronous forward method for MODULE-NAME.

This macro creates a `cl-defmethod' for `dsel-aforward' that:
1. Handles the argument list naturally (supports &rest, named params, etc.)
2. Creates local bindings for submodules accessible via `this'
3. Wraps the BODY in `dsel-aio-with-async' for async execution
4. Returns a promise that resolves to a `dsel-prediction' object

Arguments:
- MODULE-NAME: The module type this method applies to
- ARG-LIST: The parameter list (can include &rest kwargs, named params, etc.)
- DOCSTRING: Optional documentation string
- BODY: The method body, can use `dsel-aio-await' and return promises

Example:
  (dsel-defaforward my-module (query max-results)
    \"Process a query with this module.\"
    (let ((retrieved-docs (dsel-aio-await (dsel-aforward retriever :query query))))
      (dsel-aforward generator :context retrieved-docs :query query)))

  (dsel-defaforward dsel-predict (&rest kwargs)
    \"Execute prediction with kwargs.\"
    (let ((result (process-prediction kwargs)))
      result))"
  (declare (indent defun))

  ;; Handle case where docstring is actually part of body
  (when (or (null docstring) (not (stringp docstring)))
    (setq body (cons docstring body))
    (setq docstring nil))

  (let* ((method-docstring (or docstring (format "Asynchronously execute %s." module-name)))
         ;; Generate submodule bindings from `this`
         (submodule-bindings
          `((submodules (dsel-module-submodules this)))))

    `(cl-defmethod dsel-aforward ((this ,module-name) ,@arg-list)
       ,method-docstring
       (let ,submodule-bindings
         ;; Execute body inside dsel-aio-with-async
         (dsel-aio-with-async
           ,@body)))))

(provide 'dsel-module)
;;; dsel-module.el ends here
