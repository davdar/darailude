.PHONY: default sandbox-init sandbox-clean build clean html

default: build

sandbox-init:
	cabal sandbox init
	cabal install --dependencies-only

sandbox-clean:
	rm -rf .cabal-sandbox cabal.sandbox.config

build:
	cabal build

clean:
	cabal clean
