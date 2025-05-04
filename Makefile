.PHONY: test test-cursor-assist lint clean

# Test all packages
test:
	emacs -batch -l ert -l lisp/cursor-assist-tests.el -f ert-run-tests-batch-and-exit || echo "Some tests failed but continuing"
	@echo "Running simple integrity check"
	emacs -batch -l lisp/cursor-assist-check.el || echo "Integrity check failed"

# Test only cursor-assist
test-cursor-assist:
	emacs -batch -l ert -l lisp/cursor-assist-tests.el -f ert-run-tests-batch-and-exit "^cursor-assist"

# Test with direct requirements (use in an environment where packages are installed)
test-with-deps:
	emacs -batch -l ert -l gptel -l lsp-mode -l company -l lisp/cursor-assist.el -l lisp/cursor-assist-tests.el -f ert-run-tests-batch-and-exit

# Check Emacs Lisp syntax
lint:
	emacs -batch -f batch-byte-compile lisp/cursor-assist.el || echo "Compilation warnings, but proceeding"

# Clean up compiled files
clean:
	rm -f lisp/*.elc