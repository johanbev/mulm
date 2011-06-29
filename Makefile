clean:
	rm src/lisp/*.fasl
quicklisp:
	@mkdir -p lib
	@wget 'http://beta.quicklisp.org/quicklisp.lisp'
	@mv quicklisp.lisp lib
