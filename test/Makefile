all: Test TestMesg

Test: Test.hs
	ghc --make -Wall Test.hs -o Test

TestMesg: TestMesg.hs
	ghc --make -Wall TestMesg.hs -o TestMesg

lint:
	hlint -c *.hs

clean:
	rm -rf Test TestMesg
	rm -rf *.hi *.o
	rm -rf *~

.PHONY: lint clean
