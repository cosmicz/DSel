;;; dsel-optimizers.el --- Optimizers for dsel modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file provides optimizers for dsel modules, which improve LLM
;; performance through techniques like few-shot learning and bootstrapping.

;;; Code:

(require 'cl-lib)
(require 'dsel-types)
(require 'dsel-module)
(require 'dsel-predictors)
(require 'dsel-settings)

(cl-defstruct dsel-optimizer
  "Base optimizer structure."
  metric)                               ; Function: (gold-example predicted-value) -> score-or-boolean

(cl-defgeneric dsel-compile (optimizer student &rest plist)
  "Compile STUDENT module using OPTIMIZER and return optimized module.
PLIST may include:
- :trainset List of examples for training
- :teacher Teacher module (optional)
- :eval-kwargs Extra arguments for evaluation")

(cl-defstruct (dsel-labeled-fewshot (:include dsel-optimizer))
  "Optimizer that uses labeled examples for few-shot learning."
  k)                                    ; Integer: max number of labeled examples

(cl-defmethod dsel-compile ((optimizer dsel-labeled-fewshot) student &rest plist)
  "Compile STUDENT with labeled few-shot examples from PLIST trainset."
  (let* ((trainset (plist-get plist :trainset))
         (optimized-student (dsel-module-reset-copy student))
         (k (dsel-labeled-fewshot-k optimizer)))
    
    ;; For each predictor, add demos
    (dolist (predictor (dsel-collect-predictors optimized-student))
      (when (dsel-predict-p predictor)
        (let ((num-demos-to-take (min k (length trainset))))
          ;; Take the first k examples as demos
          (setf (dsel-predict-demos predictor)
                (cl-subseq (copy-sequence trainset) 0 num-demos-to-take)))))
    
    ;; Set compiled flag and return
    (setf (dsel-module-compiled-p optimized-student) t)
    optimized-student))

(defun dsel-make-labeled-fewshot (&rest plist)
  "Create a labeled few-shot optimizer from PLIST.
PLIST may include:
- :metric Function to evaluate prediction quality
- :k Max number of labeled examples"
  (make-dsel-labeled-fewshot
   :metric (plist-get plist :metric)
   :k (or (plist-get plist :k) 3)))

(cl-defstruct (dsel-bootstrap-fewshot (:include dsel-optimizer))
  "Optimizer that bootstraps examples through self-improvement."
  k-labeled                             ; Integer: max number of labeled demos
  k-bootstrapped                        ; Integer: max number of bootstrapped demos
  teacher-config)                       ; Plist: config for teacher LLM calls

(cl-defmethod dsel-compile ((optimizer dsel-bootstrap-fewshot) student &rest plist)
  "Compile STUDENT with bootstrapped examples generated from a teacher."
  (let* ((trainset (plist-get plist :trainset))
         (teacher (plist-get plist :teacher))
         (optimized-student (dsel-module-reset-copy student))
         (teacher-program (if teacher
                              (dsel-module-deepcopy teacher)
                            (dsel-module-deepcopy student)))
         (k-labeled (dsel-bootstrap-fewshot-k-labeled optimizer))
         (k-bootstrapped (dsel-bootstrap-fewshot-k-bootstrapped optimizer))
         (teacher-config (dsel-bootstrap-fewshot-teacher-config optimizer)))

    ;; Process each predictor
    (let ((student-predictors (dsel-collect-predictors optimized-student))
          (teacher-predictors (dsel-collect-predictors teacher-program)))

      (cl-loop for predictor in student-predictors
               for teacher-predictor in teacher-predictors
               when (and (dsel-predict-p predictor)
                         (dsel-predict-p teacher-predictor))
               do
               ;; Initialize with labeled examples
               (let ((labeled-demos (cl-subseq (copy-sequence trainset)
                                               0 (min k-labeled (length trainset)))))
                 (setf (dsel-predict-demos predictor) labeled-demos)

                 ;; Generate bootstrapped examples
                 (let ((num-bootstrapped-needed k-bootstrapped)
                       (remaining-trainset (cl-subseq trainset
                                                      (min k-labeled (length trainset)))))
                   ;; (message "Bootstrap DBG: Need %d more bootstrapped examples; Remaining trainset: %S" num-bootstrapped-needed remaining-trainset)
                   (dolist (train-example remaining-trainset)
                     (when (<= num-bootstrapped-needed 0)
                       ;; (message "Bootstrap DBG: No more bootstrapped examples needed")
                       (cl-return))

                     ;; Skip if already in demos
                     (when (cl-member train-example (dsel-predict-demos predictor)
                                      :test (lambda (a b)
                                              (equal (dsel-example-fields a)
                                                     (dsel-example-fields b))))
                       ;; (message "Bootstrap DBG: Already in demos, skipping")
                       (cl-continue))

                     ;; Use current student demos for teacher
                     (setf (dsel-predict-demos teacher-predictor)
                           (dsel-predict-demos predictor))

                     ;; Get inputs from the training example
                     (let* ((inputs-alist (dsel-example-inputs train-example))
                            (input-plist
                             (cl-loop for (field . value) in inputs-alist
                                      append (list field value)))
                            ;; (_ (message "Bootstrap DBG: Metric Input - Inputs: %S" input-plist))
                            ;; Have the teacher generate a prediction
                            (teacher-prediction
                             (dsel-with-settings ((lm (or (dsel-predict-lm teacher-predictor)
                                                          dsel-settings--lm)))
                               (apply #'dsel-forward
                                      teacher-predictor
                                      (append input-plist teacher-config))))

                            ;; (_ (progn
                            ;;      (message "Bootstrap DBG: Metric Input - Gold Example: %S" train-example)
                            ;;      (message "Bootstrap DBG: Metric Input - Gold fields: %S" (dsel-example-fields train-example))
                            ;;      (message "Bootstrap DBG: Metric Input - Teacher Prediction: %S" teacher-prediction)
                            ;;      (message "Bootstrap DBG: Metric Input - teacher-prediction fields: %S" (dsel-example-fields teacher-prediction))
                            ;;      ))

                            ;; Evaluate the prediction
                            (score (funcall (dsel-optimizer-metric optimizer)
                                            train-example
                                            teacher-prediction)))

                       ;; If prediction is good, add it as a demo
                       (when (or (eq score t) (and (numberp score) (> score 0)))
                         ;; Create a new demo with inputs and teacher's outputs
                         (let* ((example-inputs (dsel-example-inputs train-example))
                                (prediction-outputs (dsel-example-labels teacher-prediction))
                                (new-demo-field-args-plist
                                 (append (cl-loop for (field-symbol . value) in example-inputs
                                                  collect (dsel-symbol-to-keyword field-symbol) collect value)
                                         (cl-loop for (field-symbol . value) in prediction-outputs
                                                  collect (dsel-symbol-to-keyword field-symbol) collect value)))
                                (new-demo (apply #'dsel-make-example new-demo-field-args-plist)))
                           (setf (dsel-example-input-keys new-demo) (mapcar #'car example-inputs))
                           (push new-demo (dsel-predict-demos predictor))
                           (cl-decf num-bootstrapped-needed)))))))))

    ;; Set compiled flag and return
    (setf (dsel-module-compiled-p optimized-student) t)
    optimized-student))

(defun dsel-make-bootstrap-fewshot (&rest plist)
  "Create a bootstrap few-shot optimizer from PLIST.
PLIST may include:
- :metric Function to evaluate prediction quality
- :k-labeled Max number of labeled examples
- :k-bootstrapped Max number of bootstrapped examples
- :teacher-config Config for teacher LLM calls"
  (make-dsel-bootstrap-fewshot
   :metric (plist-get plist :metric)
   :k-labeled (or (plist-get plist :k-labeled) 3)
   :k-bootstrapped (or (plist-get plist :k-bootstrapped) 10)
   :teacher-config (plist-get plist :teacher-config)))

(provide 'dsel-optimizers)
;;; dsel-optimizers.el ends here
