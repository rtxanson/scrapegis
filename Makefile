# To build this project, make all.
#

NUMBER_OF_CORES = $(sysctl -n hw.ncpu)
BUILD_FLAGS := "-j$(NUMBER_OF_CORES)"

build_sandbox: init_sandbox
	cabal install $(BUILD_FLAGS) --only-dependencies
	cabal configure
	cabal build

doc:
	@echo "Generate haddock documentation"
	cabal haddock
	open dist/doc/html/hi/index.html

init_sandbox:
	cabal sandbox init

sandbox: init_sandbox

clean_sandbox:
	cabal sandbox delete
	rm -rf .cabal-sandbox

clean: clean_sandbox
	rm -rf dist

## Targets that most people will care about

test:
	cabal install --only-dependencies --enable-tests
	cabal test

all: build_sandbox
	cabal configure
	cabal build

global-install:
	cabal configure
	cabal deps
	cabal install --global
