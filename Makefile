
all:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs configure --user --enable-tests

doc:
	runhaskell Setup.hs haddock

ctest:
	runhaskell Setup.hs test

clean:
	runhaskell Setup.hs clean
	rm -f tests/pngsuite/*.bmp
	rm -f tests/pngsuite/*.jpg
	rm -f tests/gif/*.jpg
	rm -f tests/gif/*.png
	rm -f tests/gif/*.bmp

prof:
	dist\build\imageTest\imageTest.exe +RTS -p -sstderr -RTS test
	hp2ps -c imageTest.hp

test:
	dist\build\imageTest\imageTest.exe debug

lint:
	hlint Codec

sdist:
	runhaskell Setup.hs sdist

