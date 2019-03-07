test: build-tests
	./tests/prometheusclienttests -a --format=plain

build-tests: clean-tests
	fpc -Fupackage tests/prometheusclienttests.lpr

clean-tests:
	rm -f tests/prometheusclienttests
	rm -rf tests/lib
