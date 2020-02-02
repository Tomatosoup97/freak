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

link-curry: link-clean
	ln -s EvalTarget.hs Eval.hs
	ln -s CPSCurry.hs CPS.hs

clean: link-clean
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
	cabal install hlint
	cabal install hunit
	cabal install parsec
