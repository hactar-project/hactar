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

# Generate documentation
.PHONY: docs
docs:
	$(LISP) --load build.lisp --eval "(generate-docs)" --quit

# Run the application
.PHONY: run
run:
	./hactar

# Run the LLM CLI
.PHONY: run-cli
run-cli:
	./llm-cli

# Run tests
.PHONY: test
test:
	$(LISP) --non-interactive --load hactar.asd --load hactar-migrations.asd --load llm.asd --load hactar-tests.asd --load llm-tests.asd --eval "(ql:quickload :hactar-migrations)" --eval "(ql:quickload :hactar-tests)" --eval "(ql:quickload :llm-tests)" --load migrations/run-migrations.lisp --load run-tests.lisp --quit

# Run tests with sb-cover and generate HTML coverage report
.PHONY: coverage
coverage:
	$(LISP) --non-interactive --load hactar.asd --load hactar-migrations.asd --load llm.asd --load hactar-tests.asd --load llm-tests.asd --load run-coverage.lisp --quit
	@echo "Coverage report: coverage/cover-index.html"

# Clean build artifacts
.PHONY: clean
clean:
	rm -f hactar
	rm -f llm-cli
	rm -f *.fasl
	rm -f DOCS.md
	rm -rf coverage/

# Start Hactar in development mode with Slynk server
.PHONY: dev
dev:
	$(LISP) --load dev.lisp

# Database migrations (main DB)
.PHONY: migrate
migrate:
	$(LISP) --load hactar.asd --eval "(ql:quickload :hactar-migrations)" --load migrations/run-migrations.lisp --eval "(hactar-migrations:run-migrations)" --quit

# Database migrations (test DB)
.PHONY: migrate-test
migrate-test:
	$(LISP) --load hactar.asd hactar-migrations.asd --eval "(ql:quickload :hactar-migrations)" --load migrations/run-migrations.lisp --eval "(hactar-migrations:run-migrations :test t)" --quit

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
