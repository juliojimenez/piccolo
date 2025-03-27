SBCL=sbcl
BUILD_SCRIPT=build.lisp
OUTPUT_BINARY=piccolo

build:
	$(SBCL) --eval "(ql:quickload '(:usocket :cl-ppcre :split-sequence :quri))" --load piccolo.lisp --eval "(sb-ext:save-lisp-and-die #p\"piccolo\" :toplevel #'piccolo:run-with-args :executable t)"

clean:
	rm -f $(OUTPUT_BINARY)

ql-check:
	@$(SBCL) --non-interactive --eval "(or (find-package 'ql) (progn (format t \"Quicklisp is not installed.~%\") (sb-ext:exit :code 1)))" >/dev/null 2>&1 && echo "Quicklisp is installed." || echo "Quicklisp is missing. Run make ql-install."

ql-install:
	curl -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(SBCL) --non-interactive --no-sysinit --no-userinit \
		--load /tmp/ql.lisp \
		--eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
		--eval '(ql:add-to-init-file)' \
		--quit

.PHONY: build clean ql-check ql-install
