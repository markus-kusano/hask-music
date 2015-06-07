# Directory to put t binary
INSTALL_LOC=./install
# Name of the package. This determines where the binary is built
PACKAGE_NAME=Ch02
GHCOPTS='-Wall'
#GHCOPTS='-Wall -prof -auto-all -caf-all'
#INSTALLOPTS='--enable-library-profiling'

all:
	cabal sandbox init
	cabal install $(INSTALLOPTS) --ghc-options=$(GHCOPTS) --only-dependencies
	cabal build --ghc-options=$(GHCOPTS)
	mkdir -p $(INSTALL_LOC)
	cp dist/build/$(PACKAGE_NAME)/$(PACKAGE_NAME) $(INSTALL_LOC)

run:
	$(INSTALL_LOC)/$(PACKAGE_NAME)
