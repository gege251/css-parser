build:
	stack build

install:
	stack build
	stack install

run:
	stack build
	stack exec -- css-parser-exe
