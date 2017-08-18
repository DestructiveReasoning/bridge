SRCDIR="src/"
BUILDDIR="build/"
BINDIR="bin/"

all: build

build: init
	ghc $(SRCDIR)*.hs -odir $(BUILDDIR) -hidir $(BUILDDIR) -o $(BINDIR)bridge

init:
	mkdir -p $(BINDIR) $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)
