# Copyright 2008 Markus Dittrich <markusle@gmail.com>
# Distributed under the terms of the GNU General Public License v3

VERSION=0.4
DESTDIR=
mandir=$(DESTDIR)/usr/share/man/man1
docdir=$(DESTDIR)/usr/share/doc/husky-$(VERSION)
htmldir=$(docdir)/html
bindir=$(DESTDIR)/usr/bin

GHC_FLAGS_DEVEL = -O -Wall -fwarn-simple-patterns -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-implicit-prelude -Werror -fno-warn-orphans
GHC_FLAGS_RELEASE = -O2

OBJECTS = src/husky.hs src/CalculatorParser.hs src/CalculatorState.hs \
	  src/ExtraFunctions.hs src/HelpParser.hs src/InfoRoutines.hs \
	  src/Parser.hs src/PrettyPrint.hs src/TokenParser.hs \
	  src/UnitConverter.hs src/UnitConversionParser.hs
	  

all: debug

husky: $(OBJECTS) 
	ghc -i./src $(GHC_FLAGS_RELEASE) --make src/husky.hs


debug: $(OBJECTS) 
	ghc -i./src $(GHC_FLAGS_DEVEL) --make src/husky.hs


check: $(OBJECTS)
	ghc -i./src --make test/CalculatorTest.hs
	ghc -i./src --make test/ConverterTest.hs
	./test/CalculatorTest
	./test/ConverterTest


install: husky
	install -d $(docdir)
	install -d $(bindir)
	install -d $(htmldir)
	install -m 0755 src/husky $(bindir)/
	install -m 0644 ChangeLog COPYING AUTHORS $(docdir)/
	install -m 0644 doc/usage.html $(htmldir)/


.PHONY: clean

clean:
	rm -f src/*.o src/*.hi src/husky test/*.o test/*.hi \
		test/CalculatorTest
