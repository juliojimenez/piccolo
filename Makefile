SBCL=sbcl
BUILD_SCRIPT=build.lisp
OUTPUT_BINARY=piccolo

build:
	$(SBCL) --eval "(ql:quickload '(:usocket :cl-ppcre :split-sequence))" --load piccolo.lisp --eval "(sb-ext:save-lisp-and-die #p\"piccolo\" :toplevel #'piccolo:run :executable t)"

clean:
	rm -f $(OUTPUT_BINARY)

ql-check:
	@$(SBCL) --non-interactive --eval "(or (find-package 'ql) (progn (format t \"Quicklisp is not installed.~%\") (sb-ext:exit :code 1)))" >/dev/null 2>&1 && echo "Quicklisp is installed." || echo "Quicklisp is missing. Install it first."


.PHONY: build clean ql-check