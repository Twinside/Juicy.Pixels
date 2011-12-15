
all:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs configure --user 

doc:
	runhaskell Setup.hs haddock

clean:
	rm -f tests/pngsuite/*.bmp

run:
	dist\build\imageTest\imageTest.exe tests/jpeg/16x16jpeg.jpg

test:
	dist\build\imageTest\imageTest.exe tests/jpeg/sheep.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/avatar.jpg

