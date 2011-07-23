all: Test

Test: Test.hs
	ghc --make -Wall Test.hs -o Test

lint: Test.hs
	hlint -c Test.hs

clean:
	rm -rf Test
	rm -rf *.hi *.o
	rm -rf *~

.PHONY: lint clean
