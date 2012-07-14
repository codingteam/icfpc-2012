GHC=ghc --make -isrc/
TESTS=src/parserTest src/handlerTest src/EmulatorTest src/TestBruteforce

all: tests src/lifter

tests: $(TESTS)

src/%: src/%.hs
	$(GHC) $<

clean:
	rm -f $(TESTS)
	find . -name *.hi -delete
	find . -name *.o -delete
