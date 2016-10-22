.PHONY: setup

NODE_BIN=node_modules/.bin
TSC=$(NODE_BIN)/tsc
TYPINGS=$(NODE_BIN)/typings

all: setup haskell js

setup:
	npm install
	$(TYPINGS) install

haskell:
	stack build

js:
	$(TSC)
