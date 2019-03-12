build: clean
	fpc -Fu../../package simplewebserver.pas

serve: build
	./simplewebserver

clean:
	rm -f simplewebserver
	rm -f *.ppu
	rm -f *.o
