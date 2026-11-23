# Hactar — build, test, and run entry points.
# All targets go through these files; do not invoke sbcl/asdf directly.
LISP ?= sbcl

.PHONY: all
all: help

# Build the Hactar executable -> ./hactar/build/hactar
.PHONY: build
build:
	$(LISP) --non-interactive --load build.lisp --eval '(build-hactar)' --quit

# Run the built executable
.PHONY: run
run:
	./hactar/build/hactar

# Run the full test suite (engine + split libraries), network-free
.PHONY: test
test:
	$(LISP) --non-interactive --load run-tests.lisp

# Verify each split library loads independently (in its own fresh process)
.PHONY: test-libs
test-libs:
	@for sys in hactar-fzf hactar-md hactar-org hactar-llm hactar-llm-live hactar-fern hactar-jsonrpc hactar-vfs hactar-watch hactar-sandbox hactar-tui hactar-core hactar-codegen hactar-acp hactar-mcp hactar-http hactar-proxy hactar-rag hactar-knowledge hactar-modes-web hactar-modes-ruby hactar-modes-systems hactar-modes-lisp hactar-modes-shell hactar-modes-services hactar-starters hactar hactarsh ruhe hyperfractal hactar-pro; do \
	  $(LISP) --non-interactive --eval '(require :asdf)' \
	    --eval '(let ((q (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file q) (load q)))' \
	    --eval "(asdf:load-asd (truename \"$$sys.asd\"))" \
	    --eval "(handler-case (progn (ql:quickload (read-from-string \"$$sys\") :silent t) (format t \"~&LIB-OK $$sys~%\")) (error (e) (format t \"~&LIB-ERR $$sys ~A~%\" e) (uiop:quit 1)))" \
	    --quit || exit 1; \
	done
	@echo "All systems load independently."

# Start a development REPL with all systems loaded
.PHONY: dev
dev:
	$(LISP) --load dev.lisp

# Clear the SBCL FASL cache to force recompilation
.PHONY: clean-cache
clean-cache:
	rm -rf ~/.cache/common-lisp/

# Remove build artifacts
.PHONY: clean
clean:
	rm -rf hactar/build/
	rm -f *.fasl
	rm -rf ~/.cache/common-lisp/

.PHONY: help
help:
	@echo "Hactar targets:"
	@echo "  build      - Build ./hactar/build/hactar"
	@echo "  run        - Run the built executable"
	@echo "  test       - Run the full test suite (network-free)"
	@echo "  test-libs  - Verify each system loads independently"
	@echo "  dev        - Start a development REPL"
	@echo "  clean      - Remove build artifacts"
	@echo "  clean-cache- Clear the SBCL FASL cache"
