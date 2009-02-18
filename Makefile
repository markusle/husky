# Copyright 2008 Markus Dittrich <markusle@gmail.com>
# Distributed under the terms of the GNU General Public License v3

VERSION=0.1
DESTDIR=
mandir=$(DESTDIR)/usr/share/man/man1
docdir=$(DESTDIR)/usr/share/doc/husky-$(VERSION)
htmldir=$(docdir)/html
bindir=$(DESTDIR)/usr/bin

GHC_FLAGS_DEVEL = -O -fwarn-incomplete-patterns -fwarn-incomplete-record-updates -fwarn-missing-fields -fwarn-missing-methods -fwarn-missing-signatures -fwarn-name-shadowing -fwarn-orphans -fwarn-overlapping-patterns -fwarn-simple-patterns -fwarn-tabs -fwarn-type-defaults -fwarn-monomorphism-restriction -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches
GHC_FLAGS_RELEASE = -O2

OBJECTS = src/husky.hs src/CalculatorParser.hs src/CalculatorState.hs \
	  src/PrettyPrint.hs src/TokenParser.hs

all: husky

husky: $(OBJECTS)
	ghc -i./src $(GHC_FLAGS_RELEASE) --make src/husky.hs


debug: $(OBJECTS)
	ghc -i./src $(GHC_FLAGS_DEVEL) --make src/husky.hs


check: $(OBJECTS)
	ghc -i./src --make test/PropertyTest.hs
	./test/PropertyTest

install: husky
	install -d $(docdir)
	install -d $(bindir)
	install -d $(htmldir)
	install -m 0755 src/husky $(bindir)/
	install -m 0644 COPYING AUTHORS $(docdir)/
	install -m 0644 doc/usage.html $(htmldir)/


.PHONY: clean

clean:
	rm -f src/*.o src/*.hi src/husky test/*.o test/*.hi \
		test/PropertyTest
