;;; dsel.el --- DSPy-inspired framework for Large Language Models  -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: 
;; Keywords: llm, tools
;; Package-Requires: ((emacs "28.1") (llm "0.1"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; DSel (Diesel) is a DSPy-inspired framework for Emacs Lisp that provides
;; a structured way to build applications powered by Large Language Models (LLMs).
;; It leverages the existing `llm.el` library for backend LLM interactions.
;;
;; Key features:
;; - Define LLM tasks declaratively using "signatures"
;; - Compose tasks into larger "modules" or programs
;; - Automatically optimize programs using "optimizers"
;; - Maintain separation between program logic, prompting, and LLM execution

;;; Code:

(require 'cl-lib)
(require 'llm)

;; Load DSel modules
(require 'dsel-types)     ; Core data structures
(require 'dsel-settings)  ; Configuration and settings
(require 'dsel-adapter)   ; Adapter between DSel and llm.el
(require 'dsel-module)    ; Module base for building LLM applications
(require 'dsel-predictors) ; Predictor modules for LLM interactions
(require 'dsel-optimizers) ; Optimizers for improving LLM performance

(provide 'dsel)
;;; dsel.el ends here