warnings = -Wall \
	   -fwarn-incomplete-record-updates \
	   -fwarn-monomorphism-restriction \
	   -fwarn-tabs \
	   -fwarn-unused-do-bind \
	   -fno-warn-orphans \

flags = -isrc -odir build -hidir build

make_main = --make $(flags) $(warnings) -o build/$(exename) src/Main.hs

build:
	ghc -rtsopts $(make_main)

release:
	ghc -O3 $(make_main)

ghci:
	ghci $(warnings) $(flags) src/Tests.hs

clean:
	rm -r build/*

ctags:
	echo ":ctags" | ghci -isrc/:build/ -v0 src/TestProperties.hs

lint:
	hlint src -c

.PHONY: build clean ctags lint
