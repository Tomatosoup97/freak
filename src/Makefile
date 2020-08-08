build:
	ghc Main.hs -o freak

link:
	ln -fs $(shell pwd)/freak /usr/local/bin/freak

link-clean:
	rm -f Eval.hs
	rm -f CPS.hs

link-appel: link-clean
	ln -s EvalCPS.hs Eval.hs
	ln -s CPSAppel.hs CPS.hs

link-lists: link-clean
	ln -s EvalTarget.hs Eval.hs
	ln -s CPSLists.hs CPS.hs

clean:
	rm -f freak
	rm -f Tests
	find . -name "*.hi" -type f -delete
	find . -name "*.o" -type f -delete

lint:
	hlint .

test:
	runhaskell Tests.hs

check: build lint test

install:
	cabal install parsec
	cabal install hlint
	cabal install HUnit
	cabal install QuickCheck
	cabal install hspec
