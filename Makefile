export SHELL = /bin/bash

build:
	hlint Main.hs
	stylish-haskell -i Main.hs
	stack build $(STACK_OPTS) --copy-bins

dev:
	ghcid --command "stack ghci pixelsort"
