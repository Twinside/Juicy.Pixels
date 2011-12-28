
all:
	runhaskell Setup.hs build

conf:
	runhaskell Setup.hs configure --user 

doc:
	runhaskell Setup.hs haddock

clean:
	runhaskell Setup.hs clean
	rm -f tests/pngsuite/*.bmp

run:
	dist\build\imageTest\imageTest.exe tests/jpeg/sheep.jpg

prof:
	dist\build\imageTest\imageTest.exe +RTS -hc -p -sstderr -RTS tests/jpeg/sheep.jpg

test:
	dist\build\imageTest\imageTest.exe tests/jpeg/8x8jpeg.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/16x16jpeg.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/avatar.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/sheep.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/explore_jpeg.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/JPEG_example_JPG_RIP_001.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/JPEG_example_JPG_RIP_010.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/JPEG_example_JPG_RIP_025.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/JPEG_example_JPG_RIP_050.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/JPEG_example_JPG_RIP_100.jpg
	dist\build\imageTest\imageTest.exe tests/jpeg/fenek.jpg

lint:
	hlint Codec
