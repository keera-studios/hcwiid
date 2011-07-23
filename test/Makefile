all: test

test: Test.hs
	ghc --make -Wall Test.hs -o test

lint: Test.hs
	hlint -c Test.hs

clean:
	rm -rf test
	rm -rf *.hi *.o
	rm -rf *~

.PHONY: lint clean
