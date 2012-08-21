warnings = -Wall \
	   -fwarn-incomplete-record-updates \
	   -fwarn-monomorphism-restriction \
	   -fwarn-tabs \
	   -fwarn-unused-do-bind \
	   -fno-warn-orphans \

flags = -imath-lib/src:src -odir build -hidir build

make_main = --make $(flags) $(warnings) -o build/main src/Main.hs

build:
	ghc -rtsopts $(make_main)

release:
	ghc -O3 $(make_main)

ghci:
	ghci $(warnings) $(flags) tests/Tests.hs

clean:
	rm -r build/*

ctags:
	echo ":ctags" | ghci $(flags) -v0 */*.hs

lint:
	hlint src -c

.PHONY: build clean ctags lint
