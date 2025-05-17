;;; sentiment-classifier.el --- Example DSel sentiment classifier  -*- lexical-binding: t; -*-

;; This is an example of how to use DSel to create a sentiment classifier

;;; Code:

(require 'dsel)

;; Configure DSel to use the default LLM provider from llm.el
(dsel-configure :adapter (make-dsel-default-chat-adapter))

;; Define the signature for our sentiment classification task
(setq sentiment-signature
      (dsel-make-signature
       "Classify the sentiment of the given text as positive, negative, or neutral."
       :name 'sentiment-classifier
       :input-fields '((text . (:type string :desc "The text to classify")))
       :output-fields '((sentiment . (:type string :desc "The sentiment: positive, negative, or neutral")))))

;; Create a basic predictor using the signature
(setq sentiment-predictor
      (dsel-make-predict
       sentiment-signature
       :name 'sentiment-predictor
       :config '(:temperature 0.2)))

;; Add some few-shot examples to improve performance
(setq examples
      (list
       (dsel-example-with-inputs
        (dsel-make-example
         :text "I love this product!"
         :sentiment "positive")
        'text)
       (dsel-example-with-inputs
        (dsel-make-example
         :text "I hate this product!"
         :sentiment "negative")
        'text)
       (dsel-example-with-inputs
        (dsel-make-example
         :text "This product is okay."
         :sentiment "neutral")
        'text)))

;; Set the examples as demos for our predictor
(setf (dsel-predict-demos sentiment-predictor) examples)

;; Function to classify text using our predictor
(defun classify-sentiment (text)
  "Classify the sentiment of TEXT using our DSel predictor."
  (let* ((prediction (dsel-forward sentiment-predictor :text text))
         (sentiment (dsel-example-field prediction 'sentiment)))
    (message "Text: %s\nSentiment: %s" text sentiment)
    sentiment))

;; Create a chain-of-thought version for more complex analysis
(setq cot-predictor
      (dsel-make-chain-of-thought
       sentiment-signature
       :name 'cot-sentiment-predictor
       :rationale-field-name 'reasoning
       :rationale-field-desc "Explain why you classified the text this way"))

;; Function to classify with reasoning
(defun classify-sentiment-with-reasoning (text)
  "Classify the sentiment of TEXT with reasoning using our chain-of-thought predictor."
  (let* ((prediction (dsel-forward cot-predictor :text text))
         (reasoning (dsel-example-field prediction 'reasoning))
         (sentiment (dsel-example-field prediction 'sentiment)))
    (message "Text: %s\nReasoning: %s\nSentiment: %s" text reasoning sentiment)
    (list :sentiment sentiment :reasoning reasoning)))

;; Usage examples:
;; (classify-sentiment "I really enjoyed using this product!")
;; (classify-sentiment-with-reasoning "The product works as expected, but it's more expensive than alternatives.")

(provide 'sentiment-classifier)
;;; sentiment-classifier.el ends here