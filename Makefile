
all:
	cabal build

conf:
	cabal configure

profconf:
	cabal configure --enable-library-profiling --enable-executable-profiling


doc: docimages/pixelgraph.svg
	runhaskell Setup.hs haddock


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
	hlint lint src \
		--cpp-define=MIN_VERSION_transformers=1 \
		--cpp-define=MIN_VERSION_binary=1

sdist: docimages/pixelgraph.svg
	runhaskell Setup.hs sdist

docimages/pixelgraph.svg: docimages/pixelgraph.gv
	dot -Tsvg -o docimages/pixelgraph.svg docimages/pixelgraph.gv

