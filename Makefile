GHC=ghc --make -isrc/
TESTS=src/parserTest src/handlerTest src/EmulatorTest src/TestBruteforce
NUMBER=95597812
PACKAGE=icfp-$(NUMBER).tgz

all: tests src/lifter

tests: $(TESTS)

src/%: src/%.hs
	$(GHC) $<

pack: 
	tar fvcz $(PACKAGE) install PACKAGES-TESTING src/*.hs README

clean:
	rm -f $(TESTS) src/lifter
	rm -f $(PACKAGE)
	find . -name *.hi -delete
	find . -name *.o -delete
