test: build-tests
	./tests/prometheusclienttests -a --format=plain

build-tests: clean-tests
	fpc tests/prometheusclienttests.lpr

clean-tests:
	rm -f tests/prometheusclienttests
	rm -rf tests/lib
