;;; sentiment-classifier.el --- Example DSel sentiment classifier  -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Cosmin-Octavian C. (cosmicz)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This example demonstrates how to use DSel to create a simple sentiment classifier,
;; including basic prediction, chain-of-thought reasoning, and few-shot optimization.

;;; Code:

(require 'dsel)
(require 'dsel-settings)

;; Define a signature for sentiment classification
(dsel-defsignature sentiment-signature
    "Classify the sentiment of the given text as positive, negative, or neutral."
  :input-fields
  '((:name text
           :type string
           :desc "The text to classify"
           :prefix "TEXT:"))
  :output-fields
  '((:name sentiment
           :type string
           :desc "The sentiment of the text"
           :prefix "SENTIMENT:"
           :enum ["positive" "negative" "neutral"])))

;; Create few-shot examples to improve performance
(defvar sentiment-examples
  (dsel-create-examples
   '((:text "I love this product! It's amazing and exceeded my expectations."
            :sentiment "positive")
     (:text "I'm extremely disappointed with this purchase. It broke after one day."
            :sentiment "negative")
     (:text "The product functions as described. It's neither impressive nor disappointing."
            :sentiment "neutral")
     (:text "This is the best purchase I've made all year! Highly recommend it."
            :sentiment "positive")
     (:text "Terrible customer service and the product quality is poor."
            :sentiment "negative"))
   :input-keys '(text))
  "Few-shot examples for sentiment classification.")

;; Create a basic predictor with temperature=0.2 for more consistent outputs
(dsel-defpredict sentiment-predictor sentiment-signature
  :config '(:temperature 0.2)
  :demos sentiment-examples)

;; Function to classify text using our predictor
(defun classify-sentiment (text)
  "Classify the sentiment of TEXT using our DSel predictor."
  (let ((prediction (dsel-forward sentiment-predictor :text text)))
    (if (dsel-prediction-ok-p prediction)
        (let ((sentiment (dsel-get-field prediction 'sentiment)))
          (message "Classified '%s' as: %s" text sentiment)
          sentiment)
      (progn
        (message "Error classifying sentiment for '%s':" text)
        (dsel-prediction-report-errors prediction)
        nil))))

;; Create a chain-of-thought version that explains its reasoning
(dsel-defchain-of-thought sentiment-cot-predictor sentiment-signature
  :rationale-field-name 'reasoning
  :rationale-field-desc "Explain why you classified the text with this sentiment"
  :rationale-field-prefix "REASONING:"
  :demos sentiment-examples
  :config '(:temperature 0.3))

;; Function to classify with reasoning
(defun classify-sentiment-with-reasoning (text)
  "Classify TEXT with reasoning using the chain-of-thought predictor."
  (let ((prediction (dsel-forward sentiment-cot-predictor :text text)))
    (if (dsel-prediction-ok-p prediction)
        (let ((reasoning (dsel-get-field prediction 'reasoning))
              (sentiment (dsel-get-field prediction 'sentiment)))
          (message "Sentiment analysis for '%s':\nResult: %s\nReasoning: %s"
                   text sentiment reasoning)
          (list :sentiment sentiment :reasoning reasoning))
      (progn
        (message "Error analyzing sentiment with reasoning for '%s':" text)
        (dsel-prediction-report-errors prediction)
        nil))))

;; Define an optimizer that selects the most relevant few-shot examples
(dsel-defoptimizer sentiment-optimizer labeled-fewshot
  :metric (lambda (gold pred)
            (string= (dsel-get-field gold 'sentiment)
                     (dsel-get-field pred 'sentiment)))
  :k 3)

;; Function to optimize a predictor for a given domain or use case
(defun optimize-sentiment-predictor (predictor trainset)
  "Optimize PREDICTOR using TRAINSET examples.
Returns a new optimized predictor instance."
  (message "Optimizing sentiment predictor with %d training examples" 
           (length trainset))
  (let ((optimized (dsel-compile sentiment-optimizer predictor :trainset trainset)))
    (message "Optimization complete")
    optimized))

;; Function to apply the optimized predictor
(defun classify-with-optimized (optimized-predictor text)
  "Classify TEXT using the OPTIMIZED-PREDICTOR."
  (let ((prediction (dsel-forward optimized-predictor :text text)))
    (if (dsel-prediction-ok-p prediction)
        (let ((sentiment (dsel-get-field prediction 'sentiment)))
          (message "Optimized classification for '%s': %s" text sentiment)
          sentiment)
      (progn
        (message "Error with optimized classification for '%s':" text)
        (dsel-prediction-report-errors prediction)
        nil))))

;; Setup function to configure settings for this example
(defun sentiment-classifier-setup (&optional temperature max-tokens)
  "Set up the sentiment classifier with custom parameters.
TEMPERATURE controls response randomness (default 0.2).
MAX-TOKENS limits token generation (if supported by provider)."
  (let ((config (list :temperature (or temperature 0.2))))
    (when max-tokens
      (setq config (plist-put config :max-tokens max-tokens)))
    
    ;; Update predictor configurations
    (setf (dsel-predict-config sentiment-predictor) config)
    (setf (dsel-predict-config sentiment-cot-predictor) 
          (plist-put (copy-sequence config) :temperature (+ 0.1 (or temperature 0.2))))

    (message "Sentiment classifier setup complete with temperature: %.1f" 
             (plist-get config :temperature))))

;; Usage examples:
;;
;; 1. Configure model parameters
;;    (sentiment-classifier-setup 0.3 150)
;;
;; 2. Simple classification
;;    (classify-sentiment "I really enjoyed using this product!")
;;
;; 3. Classification with reasoning
;;    (classify-sentiment-with-reasoning "The product works as expected, but it's more expensive than alternatives.")
;;
;; 4. Create an optimized predictor for a specific domain (e.g., book reviews)
;;    (setq book-review-examples
;;          (list (dsel-make-example :text "A real page-turner with rich characters." :sentiment "positive")
;;                (dsel-make-example :text "The plot was predictable and boring." :sentiment "negative")
;;                (dsel-make-example :text "The characters were well-developed." :sentiment "positive")))
;;    (setq book-review-predictor 
;;          (optimize-sentiment-predictor sentiment-predictor book-review-examples))
;;    (classify-with-optimized book-review-predictor "The world-building was excellent but the ending felt rushed.")

(provide 'sentiment-classifier)
;;; sentiment-classifier.el ends here
