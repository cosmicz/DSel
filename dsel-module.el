;;; dsel-module.el --- Module base for dsel  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides the module base for dsel, which is used to build
;; complex LLM applications by composing simpler modules.

;;; Code:

(require 'cl-lib)

(cl-defstruct dsel-module
  "Base module structure for building LLM application components."
  name                                  ; Symbol: name of the module instance
  (predictors nil :type list)           ; List: child modules/predictors
  compiled-p)                           ; Boolean: if the module has been optimized

(cl-defgeneric dsel-forward (module &rest kwargs)
  "Execute MODULE with KWARGS and return a prediction.
This is the main execution method for modules.")

(cl-defgeneric dsel-collect-predictors (module)
  "Recursively collect all active predictor instances from MODULE and its sub-modules.")

(cl-defmethod dsel-collect-predictors ((module dsel-module))
  "Recursively collect all predictor instances from MODULE and its sub-modules."
  (let ((collected '()))
    ;; Iterate over direct children in the 'predictors' slot
    (dolist (child (dsel-module-predictors module))
      (if (and (fboundp 'dsel-predict-p) (dsel-predict-p child))
          (push child collected)
        (when (dsel-module-p child)
          (setq collected (append (dsel-collect-predictors child) collected)))))
    (nreverse collected)))

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
         ;; Deep copy predictors recursively
         (predictor-copies 
          (mapcar (lambda (pred)
                    (if (dsel-module-p pred)
                        (dsel-module-deepcopy pred)
                      ;; For non-module objects, use a simple copy
                      (copy-sequence pred)))
                  (dsel-module-predictors module))))
    (setf (dsel-module-predictors copy) predictor-copies)
    copy))

(provide 'dsel-module)
;;; dsel-module.el ends here
