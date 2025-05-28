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

(cl-defstruct dsel-module
  "Base module structure for building LLM application components."
  name                                  ; Symbol: name of the module instance
  (submodules nil :type list)           ; Alist: (name . module) pairs for child modules/predictors
  compiled-p)                           ; Boolean: if the module has been optimized

(cl-defgeneric dsel-forward (module &rest kwargs)
  "Execute MODULE with KWARGS and return a prediction.
This is the main execution method for modules.")

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

(provide 'dsel-module)
;;; dsel-module.el ends here
