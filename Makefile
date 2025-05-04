.PHONY: test test-cursor-assist lint clean

# Test all packages
test:
	emacs -batch -l ert -l lisp/cursor-assist.el -l lisp/cursor-assist-tests.el -f ert-run-tests-batch-and-exit

# Test only cursor-assist
test-cursor-assist:
	emacs -batch -l ert -l lisp/cursor-assist.el -l lisp/cursor-assist-tests.el -f ert-run-tests-batch-and-exit "^cursor-assist"

# Check Emacs Lisp syntax
lint:
	emacs -batch -f batch-byte-compile lisp/*.el

# Clean up compiled files
clean:
	rm -f lisp/*.elc