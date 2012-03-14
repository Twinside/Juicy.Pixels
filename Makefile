
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

run:
	dist\build\imageTest\imageTest.exe tests/jpeg/sheep.jpg > out_log_file 2>&1 

prof:
	dist\build\imageTest\imageTest.exe +RTS -hy -p -sstderr -RTS tests/pngsuite/huge.png
	hp2ps -c imageTest.hp

test:
	dist\build\imageTest\imageTest.exe

lint:
	hlint Codec

sdist:
	runhaskell Setup.hs sdist
