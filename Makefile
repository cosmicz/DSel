EMACS ?= emacs

BATCH = $(EMACS) --batch -Q -L . -L ./tests

.PHONY: all compile test tests clean

all: compile

compile:
	@echo "Compiling DSel Elisp files..."
	@$(BATCH) -f batch-byte-compile *.el

# Allow either SELECT or SELECTOR to be used
SELECT ?= ^dsel-test-
SELECTOR ?= $(SELECT)

test:
	@$(BATCH) -l ./tests/dsel-tests-runner.el --eval '(dsel-run-tests-batch "$(SELECTOR)")'

# Alias for 'test' target
tests: test

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
	@echo "  tests    - Alias for 'test' target"
	@echo "             You can specify a test selector with either SELECT= or SELECTOR="
	@echo "             Example: make test SELECT=dsel-test-predict-basic"
	@echo "             Example: make tests SELECTOR=adapter"
	@echo "  clean    - Remove all .elc files and test packages"
	@echo "  help     - Show this help message"
