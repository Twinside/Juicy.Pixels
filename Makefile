
all:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs configure --user 

clean:
	rm -f tests/pngsuite/*.bmp

