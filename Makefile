EMACS ?= emacs
BATCH = $(EMACS) --batch -Q -L . -L ./tests

.PHONY: all compile test clean

all: compile

compile:
	@echo "Compiling DSel Elisp files..."
	@$(BATCH) -f batch-byte-compile *.el

test:
	@$(BATCH) -l ./tests/dsel-tests-runner.el -f dsel-run-tests-batch

clean:
	@echo "Cleaning up compilation artifacts..."
	@rm -f *.elc tests/*.elc
	@rm -rf tests/.packages
	@echo "Done."

.PHONY: help
help:
	@echo "DSel Makefile targets:"
	@echo "  all      - Default target. Same as 'compile'"
	@echo "  compile  - Byte-compile all Elisp files"
	@echo "  test     - Run tests (requires llm.el package)"
	@echo "  clean    - Remove all .elc files and test packages"
	@echo "  help     - Show this help message"