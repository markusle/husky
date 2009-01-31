# Copyright 2008 Markus Dittrich <markusle@gmail.com>
# Distributed under the terms of the GNU General Public License v3

VERSION=0.0-alpha
DESTDIR=
mandir=$(DESTDIR)/usr/share/man/man1
docdir=$(DESTDIR)/usr/share/doc/husky-$(VERSION)
htmldir=$(docdir)/html
bindir=$(DESTDIR)/usr/bin

GHC_FLAGS_DEVEL = -O -fwarn-incomplete-patterns -fwarn-incomplete-record-updates -fwarn-missing-fields -fwarn-missing-methods -fwarn-missing-signatures -fwarn-name-shadowing -fwarn-orphans -fwarn-overlapping-patterns -fwarn-simple-patterns -fwarn-tabs -fwarn-type-defaults -fwarn-monomorphism-restriction -fwarn-unused-binds -fwarn-unused-imports -fwarn-unused-matches
GHC_FLAGS_RELEASE = -O2

OBJECTS = src/ApplicativeParsec.hs src/husky.hs 


all: debug

husky: $(OBJECTS)
	ghc -i./src $(GHC_FLAGS_RELEASE) --make src/husky.hs


debug: $(OBJECTS)
	ghc -i./src $(GHC_FLAGS_DEVEL) --make src/husky.hs


test: husky
	make -C test

install:
	install -d $(docdir)
	install -d $(mandir)
	install -d $(bindir)
	install -m 0755 src/husky $(bindir)/
	install -m 0644 COPYING AUTHORS $(docdir)/


.PHONY: clean

clean:
	rm -f src/*.o src/*.hi src/husky
