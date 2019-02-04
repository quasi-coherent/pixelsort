.DEFAULT_GOAL = help

export SHELL = /bin/bash

OS = $(shell if [ "$$(uname -s)" = "Darwin" ]; then echo -n osx; else echo -n linux; fi)
LTS = $(shell egrep '^resolver:' stack.yaml | awk '{print $$2}')
GHC_VER = $(shell stack ghc -- --version | awk '{print $$NF}')
BIN_DIR := bin

help: ## Help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'

install: ## Install stack and tools
	brew upgrade
	brew install stack
	stack install hlint stylish-haskell

$(BIN_DIR):
	mkdir -p $@

build: $(BIN_DIR) ## Make distribution
	stack build $(STACK_OPTS)
	hlint Main.hs
	stylish-haskell -i Main.hs
	cp .stack-work/install/x86_64-${OS}/${LTS}/${GHC_VER}/bin/pixelsort $(BIN_DIR)/pixelsort-${OS}
