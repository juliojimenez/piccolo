SBCL=sbcl
BUILD_SCRIPT=build.lisp
OUTPUT_BINARY=piccolo

build:
	$(SBCL) --eval "(ql:quickload '(:usocket :cl-ppcre :split-sequence))" --load piccolo.lisp --eval "(sb-ext:save-lisp-and-die #p\"piccolo\" :toplevel #'piccolo:run :executable t)"

clean:
	rm -f $(OUTPUT_BINARY)

.PHONY: build clean