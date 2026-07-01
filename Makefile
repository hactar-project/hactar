# Variables
LISP ?= sbcl
TEST_SYSTEM = llm-test

.PHONY: all
all: help

# Build the executable
.PHONY: build
build:
	$(LISP) --load build.lisp --eval "(build-hactar)" --quit

# Build the LLM CLI
.PHONY: build-cli
build-cli:
	$(LISP) --load build.lisp --eval "(build-llm-cli)" --quit

# Build the greenfield rebuild engine binary -> ./hactar/build/hactar
.PHONY: build-rebuild
build-rebuild:
	$(LISP) --non-interactive --load build-rebuild.lisp --quit

# Run the greenfield rebuild test suite (network-free, no API keys required)
.PHONY: test-rebuild
test-rebuild:
	$(LISP) --non-interactive --load run-rebuild-tests.lisp

# Verify each split library loads independently (in its own fresh process)
.PHONY: test-libs
test-libs:
	@for sys in hactar-fzf hactar-md hactar-org hactar-llm hactar-fern; do \
	  $(LISP) --non-interactive --eval '(require :asdf)' \
	    --eval '(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file q) (load q)))' \
	    --eval "(asdf:load-asd (truename \"$$sys.asd\"))" \
	    --eval "(handler-case (progn (ql:quickload (read-from-string \"$$sys\") :silent t) (format t \"~&LIB-OK $$sys~%\")) (error (e) (format t \"~&LIB-ERR $$sys ~A~%\" e) (uiop:quit 1)))" \
	    --quit || exit 1; \
	done
	@echo "All split libraries load independently."

# Run the application
.PHONY: run
run:
	./hactar/build/hactar

# Run the LLM CLI
.PHONY: run-cli
run-cli:
	./llm-cli

# Run tests
.PHONY: test
test:
	$(LISP) --non-interactive --eval "(require :asdf)" --eval "(let ((q (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))) (when (probe-file q) (load q)))" --load hactar.asd --load llm.asd --load hactar-tests.asd --load llm-tests.asd --eval "(ql:quickload :hactar-tests)" --eval "(ql:quickload :llm-tests)" --load run-tests.lisp --quit

# Clear SBCL FASL cache to force recompilation
.PHONY: clean-cache
clean-cache:
	rm -rf ~/.cache/common-lisp/

# Run tests with sb-cover and generate HTML coverage report
.PHONY: coverage
coverage:
	$(LISP) --non-interactive --eval "(require :asdf)" --load hactar.asd --load llm.asd --load hactar-tests.asd --load llm-tests.asd --load run-coverage.lisp --quit
	@echo "Coverage report: coverage/cover-index.html"

# Clean build artifacts
.PHONY: clean
clean:
	rm -rf hactar/build/
	rm -f llm-cli
	rm -f *.fasl
	rm -f DOCS.md
	rm -rf coverage/
	rm -rf ~/.cache/common-lisp/

# Start Hactar in development mode with Slynk server
.PHONY: dev
dev:
	$(LISP) --load dev.lisp

# Install via Roswell
.PHONY: install-ros
install-ros:
	ros install hactarproject/hactar

# Help target
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  all      - Default target, shows this help message"
	@echo "  dev      - Start Hactar in development mode with Slynk server"
	@echo "  build    - Build the hactar executable"
	@echo "  docs     - Generate API documentation into DOCS.md"
	@echo "  run      - Run the built hactar executable"
	@echo "  run-cli  - Run the built llm-cli executable"
	@echo "  build-cli- Build the llm-cli executable"
	@echo "  test     - Run the test suite (runs migrate-test first)"
	@echo "  clean    - Remove build artifacts"
	@echo "  migrate  - Run database migrations for the main database"
	@echo "  migrate-test - Run database migrations for the test database"
	@echo "  coverage - Run tests with sb-cover and generate HTML coverage report"
	@echo "  install-ros - Install hactar via Roswell"
	@echo "  help     - Show this help message"
