test: build-tests
	./tests/prometheusclienttests -a --format=plain

build-tests: clean
	fpc -Sa -Fupackage tests/prometheusclienttests.lpr

test-leaks: build-tests-with-gh
	./tests/prometheusclienttests -a --format=plain

build-tests-with-gh: clean
	fpc -gh -Sa -Fupackage tests/prometheusclienttests.lpr

clean:
	rm -f tests/prometheusclienttests
	rm -rf tests/lib
	rm -f tests/*.ppu
	rm -f tests/*.o
	rm -rf package/lib
	rm -f package/*.ppu
	rm -f package/*.o
